LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;

ENTITY SHA1_CORE IS
    PORT (
        clk : IN STD_LOGIC;
        rst_in : IN STD_LOGIC;
        byte : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
        valid_in : IN STD_LOGIC;
        last : IN STD_LOGIC;
        hash : OUT STD_LOGIC_VECTOR(159 DOWNTO 0);
        valid_out : OUT STD_LOGIC;
        ready_in : OUT STD_LOGIC
    );
END SHA1_CORE;

ARCHITECTURE Behavioral OF SHA1_CORE IS

    CONSTANT init_h0 : STD_LOGIC_VECTOR(31 DOWNTO 0) := "01100111010001010010001100000001";
    CONSTANT init_h1 : STD_LOGIC_VECTOR(31 DOWNTO 0) := "11101111110011011010101110001001";
    CONSTANT init_h2 : STD_LOGIC_VECTOR(31 DOWNTO 0) := "10011000101110101101110011111110";
    CONSTANT init_h3 : STD_LOGIC_VECTOR(31 DOWNTO 0) := "00010000001100100101010001110110";
    CONSTANT init_h4 : STD_LOGIC_VECTOR(31 DOWNTO 0) := "11000011110100101110000111110000";

    CONSTANT k0 : STD_LOGIC_VECTOR(31 DOWNTO 0) := "01011010100000100111100110011001";
    CONSTANT k1 : STD_LOGIC_VECTOR(31 DOWNTO 0) := "01101110110110011110101110100001";
    CONSTANT k2 : STD_LOGIC_VECTOR(31 DOWNTO 0) := "10001111000110111011110011011100";
    CONSTANT k3 : STD_LOGIC_VECTOR(31 DOWNTO 0) := "11001010011000101100000111010110";

    constant one : STD_LOGIC_VECTOR(4 DOWNTO 0) := "00001";
    constant sixtyzeroes : STD_LOGIC_VECTOR(63 downto 4) := (others => '0');
    constant lenone: std_logic_vector(63 downto 3) := sixtyzeroes & '1';

    TYPE warr IS ARRAY (1 TO 16) OF STD_LOGIC_VECTOR(31 DOWNTO 0);

    SIGNAL h0 : STD_LOGIC_VECTOR(31 DOWNTO 0) := init_h0;
    SIGNAL h1 : STD_LOGIC_VECTOR(31 DOWNTO 0) := init_h1;
    SIGNAL h2: STD_LOGIC_VECTOR(31 DOWNTO 0) := init_h2;
    SIGNAL h3: STD_LOGIC_VECTOR(31 DOWNTO 0) := init_h3;
    SIGNAL h4: STD_LOGIC_VECTOR(31 DOWNTO 0) := init_h4;

    SIGNAL a : STD_LOGIC_VECTOR(31 DOWNTO 0) := init_h0;
    SIGNAL b : STD_LOGIC_VECTOR(31 DOWNTO 0) := init_h1;
    SIGNAL c : STD_LOGIC_VECTOR(31 DOWNTO 0) := init_h2;
    SIGNAL d : STD_LOGIC_VECTOR(31 DOWNTO 0) := init_h3;
    SIGNAL e : STD_LOGIC_VECTOR(31 DOWNTO 0) := init_h4;

    SIGNAL f : STD_LOGIC_VECTOR(31 DOWNTO 0);
    SIGNAL w : STD_LOGIC_VECTOR(31 DOWNTO 0) := (others => '0');
    SIGNAL k : STD_LOGIC_VECTOR(31 DOWNTO 0);

    SIGNAL ch : STD_LOGIC_VECTOR(31 DOWNTO 0);
    SIGNAL parity : STD_LOGIC_VECTOR(31 DOWNTO 0);
    SIGNAL maj : STD_LOGIC_VECTOR(31 DOWNTO 0);

    SIGNAL rotl1 : STD_LOGIC_VECTOR(31 DOWNTO 0);
    SIGNAL rotl5 : STD_LOGIC_VECTOR(31 DOWNTO 0);
    SIGNAL rotl30 : STD_LOGIC_VECTOR(31 DOWNTO 0);
    SIGNAL wregxor : STD_LOGIC_VECTOR(31 DOWNTO 0);

    SIGNAL word_low : STD_LOGIC_VECTOR(7 DOWNTO 0);
    SIGNAL word_high : STD_LOGIC_VECTOR(31 DOWNTO 8) := (others => '0');
    SIGNAL length : STD_LOGIC_VECTOR(63 DOWNTO 3); -- low 3 of length (unit: bits) is 0 
    SIGNAL wreg : warr := (others => (others => '0'));

    SIGNAL incnt4 : STD_LOGIC_VECTOR(1 DOWNTO 0);
    SIGNAL tcnt20 : STD_LOGIC_VECTOR(4 DOWNTO 0);
    SIGNAL tcnt4 : STD_LOGIC_VECTOR(1 DOWNTO 0);

    SIGNAL lasted : STD_LOGIC := '0';
    SIGNAL skip : STD_LOGIC := '0';
    SIGNAL done : STD_LOGIC := '0';
    SIGNAL hashing : STD_LOGIC;

    SIGNAL tcntunder20 : STD_LOGIC;
    SIGNAL tcntunder16 : STD_LOGIC;
    SIGNAL tcnt14or15 : STD_LOGIC;
    SIGNAL tcnt20is19 : STD_LOGIC;
    SIGNAL tcntis79 : STD_LOGIC;

    SIGNAL rst : STD_LOGIC;

BEGIN

    ch <= ((b AND c) XOR ((NOT b) AND d));
    parity <= b XOR c XOR d;
    maj <= ((b AND c) XOR (b AND d) XOR (c AND d));

    rotl1 <= wregxor(30 DOWNTO 0) & wregxor(31 DOWNTO 31);
    rotl5 <= a(26 DOWNTO 0) & a(31 DOWNTO 27);
    rotl30 <= b(1 DOWNTO 0) & b(31 DOWNTO 2);
    wregxor <= wreg(3) XOR wreg(8) XOR wreg(14) XOR wreg(16);

    hashing <= incnt4(1) and incnt4(0) and ((not tcntunder16) or valid_in or lasted);
    ready_in <= (tcntunder16 and (not done));
    rst <= rst_in;
    hash <= h0 & h1 & h2 & h3 & h4;
    valid_out <= done;

    tcntunder20 <= tcnt4(1) NOR tcnt4(0);
    tcntunder16 <= tcntunder20 AND (NOT tcnt20(4));
    tcnt14or15 <= tcntunder16 AND tcnt20(3) AND tcnt20(2) AND tcnt20(1);
    tcnt20is19 <= (tcnt20(4) AND tcnt20(1) AND tcnt20(0));
    tcntis79 <= tcnt4(1) AND tcnt4(0) AND tcnt20is19;

    F_CTRL : PROCESS (tcnt4, ch, parity, maj)
    BEGIN
        CASE(tcnt4) IS
            WHEN "00" => f <= ch;
            WHEN "01" => f <= parity;
            WHEN "10" => f <= maj;
            WHEN "11" => f <= parity;
            WHEN OTHERS =>
        END CASE;
    END PROCESS;

    K_CTRL : PROCESS (tcnt4)
    BEGIN
        CASE(tcnt4) IS
            WHEN "00" => k <= k0;
            WHEN "01" => k <= k1;
            WHEN "10" => k <= k2;
            WHEN "11" => k <= k3;
            WHEN OTHERS =>
        END CASE;
    END PROCESS;

    W_CTRL : PROCESS (word_high, word_low, tcntunder16 , tcnt14or15, tcnt20, skip, length, rotl1)
    BEGIN
        IF (tcntunder16 = '1') THEN
            IF (((NOT skip) AND tcnt14or15) = '1') THEN
                IF (tcnt20(0) = '1') THEN
                    w <= length(31 DOWNTO 3) & "000";
                ELSE
                    w <= length(63 DOWNTO 32);
                END IF;
            ELSE
                w <= word_high & word_low;
            END IF;
        ELSE
            w <= rotl1;
        END IF;
    END PROCESS;

    FLAG_CTRL : PROCESS (clk, rst)
    BEGIN
        IF (rising_edge(clk)) THEN
            IF (rst = '1') THEN
                lasted <= '0';
                skip <= '0';
                done <= '0';
            ELSE
                IF ((valid_in AND last) = '1') THEN
                    lasted <= '1';
                    IF (tcnt14or15 = '1') THEN
                        skip <= '1';
                    END IF;
                END IF;
                IF (tcntis79 = '1') THEN
                    IF (skip = '1') THEN
                        skip <= '0';
                    ELSE
                        done <= '1';
                    END IF;
                END IF;
                if ((valid_in and done) = '1') then
                    done <= '0';
                end if;
            END IF;
        END IF;
    END PROCESS;

    WLOW_CTRL : PROCESS (byte, last, lasted)
    BEGIN
        IF (lasted = '1') THEN
            word_low <= (OTHERS => '0');
        ELSIF (last = '1') THEN
            word_low <= "10000000";
        ELSE
            word_low <= byte;
        END IF;
    END PROCESS;

    SFT_CTRL : PROCESS (clk)
    BEGIN
        IF (rising_edge(clk)) THEN
            IF (hashing = '1') THEN
                for i in 1 to 15 loop
                    wreg(i+1) <= wreg(i);
                end loop;
                wreg(1) <= w;
            End if;
            if ((valid_in or lasted) = '1') then
                word_high(31 DOWNTO 24) <= word_high(23 DOWNTO 16);
                word_high(23 DOWNTO 16) <= word_high(15 DOWNTO 8);
                word_high(15 DOWNTO 8) <= word_low;
            END IF;
        END IF;
    END PROCESS;

    LEN_CTRL : PROCESS (clk, rst)
    BEGIN
        IF (rising_edge(clk)) THEN
            IF (rst = '1') THEN
                length <= (OTHERS => '0');
            ELSIF ((valid_in AND (NOT last) AND (NOT lasted)) = '1') THEN
                length <= std_logic_vector(unsigned(length) + unsigned(lenone));
            END IF;
        END IF;
    END PROCESS;

    INCNT_CTRL : PROCESS (clk, rst)
    BEGIN
        IF (rising_edge(clk)) THEN
            IF (rst = '1') THEN
                incnt4 <= (OTHERS => '0');
            ELSE
                if ((hashing and tcnt20(3) and tcnt20(2) and tcnt20(1) and tcnt20(0))= '1') then
                    incnt4 <= "11";
                elsif (tcntis79 = '1') then
                        incnt4 <= "00";
                elsif ((tcntunder16 and (valid_in or (lasted and (not done)))) = '1') THEN
                    ----- incnt4++ -----
                    -- (1)- | (0)- | (1)+ | (0)+ | 
                    --  0   |  0   |  0   |  1   |
                    --  0   |  1   |  1   |  0   |
                    --  1   |  0   |  1   |  1   |
                    --  1   |  1   |  0   |  0   |
                    incnt4(1) <= incnt4(1) XOR incnt4(0);
                    incnt4(0) <= NOT incnt4(0);
                END IF;
            END IF;
        END IF;
    END PROCESS;

    TCNT_CTRL : PROCESS (clk, rst)
    BEGIN
        IF (rising_edge(clk)) THEN
            IF (rst = '1') THEN
                tcnt20 <= (OTHERS => '0');
                tcnt4 <= (OTHERS => '0');
            ELSE
                IF ((hashing and (not done)) = '1') THEN
                    IF (tcnt20is19 = '1') THEN
                        -- tcnt20 = "1XX11" = 19
                        ----- tcnt4++ -----
                        -- (1)- | (0)- | (1)+ | (0)+ | 
                        --  0   |  0   |  0   |  1   |
                        --  0   |  1   |  1   |  0   |
                        --  1   |  0   |  1   |  1   |
                        --  1   |  1   |  0   |  0   |
                        tcnt4(1) <= tcnt4(1) XOR tcnt4(0);
                        tcnt4(0) <= NOT tcnt4(0);
                        -------------------
                        tcnt20 <= (OTHERS => '0');
                    ELSE
                        tcnt20 <= STD_LOGIC_VECTOR(unsigned(tcnt20) + unsigned(one));
                    END IF;
                END IF;
            END IF;
        END IF;
    END PROCESS;

    HASH_CTRL : PROCESS (clk, rst)
    BEGIN
        IF (rising_edge(clk)) THEN
            IF (rst = '1') THEN
                a <= init_h0;
                b <= init_h1;
                c <= init_h2;
                d <= init_h3;
                e <= init_h4;
                h0 <= init_h0;
                h1 <= init_h1;
                h2 <= init_h2;
                h3 <= init_h3;
                h4 <= init_h4;
            ELSE
                IF ((hashing AND (NOT done)) = '1') THEN
                    if (tcntis79 = '1') then
                        a <= std_logic_vector(unsigned(h0) + unsigned(e) + unsigned(f) + unsigned(rotl5) + unsigned(w) + unsigned(k));
                        b <= std_logic_vector(unsigned(h1) + unsigned(a));
                        c <= std_logic_vector(unsigned(h2) + unsigned(rotl30));
                        d <= std_logic_vector(unsigned(h3) + unsigned(c));
                        e <= std_logic_vector(unsigned(h4) + unsigned(d));
                        h0 <= std_logic_vector(unsigned(h0) + unsigned(e) + unsigned(f) + unsigned(rotl5) + unsigned(w) + unsigned(k));
                        h1 <= std_logic_vector(unsigned(h1) + unsigned(a));
                        h2 <= std_logic_vector(unsigned(h2) + unsigned(rotl30));
                        h3 <= std_logic_vector(unsigned(h3) + unsigned(c));
                        h4 <= std_logic_vector(unsigned(h4) + unsigned(d));
                    else
                    a <= STD_LOGIC_VECTOR(unsigned(e) + unsigned(f) + unsigned(rotl5) + unsigned(w) + unsigned(k));
                    b <= a;
                    c <= rotl30;
                    d <= c;
                    e <= d;
                    end if;
                END IF;
            END IF;
        END IF;
    END PROCESS;

END Behavioral;
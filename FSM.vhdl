library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
library UNISIM;
use UNISIM.VComponents.all;

entity fsm is
    Port ( clk : in STD_LOGIC;
           rst : in STD_LOGIC;
           sw : in STD_LOGIC_VECTOR (15 downto 0);
           led : out STD_LOGIC_VECTOR (15 downto 0);
           btnC : in std_logic;
           an : out std_logic_vector (3 downto 0);
           seg : out std_logic_vector (0 to 6);
           dp : out STD_LOGIC
         );
end fsm;

architecture Behavioral of fsm is

type int_array is array(0 to 3) of integer;
signal rnd : STD_LOGIC_VECTOR (2 downto 0);
signal diceValues:  int_array;
signal diceToReroll: int_array;
signal rerollTarget: integer := 0;
signal rerollCurrent: integer := 0;
signal currentDice: integer := 0;
signal userDiceSum: int_array;
signal userScores: int_array := (0,0,0,0);
signal player_change: std_logic := '0';
signal userId: integer := 0;
type states is (start, get_rnd, show, user_select, reroll, dice_value_calculation ,score_calculation, show_score);
signal current_state, next_state : states;
signal Din : STD_LOGIC_VECTOR (15 downto 0); 
signal dp_in : STD_LOGIC_VECTOR (3 downto 0) := "0000";

component driver7seg is
    Port ( clk : in STD_LOGIC; --100MHz board clock input
           Din : in STD_LOGIC_VECTOR (15 downto 0); --16 bit binary data for 4 displays
           an : out STD_LOGIC_VECTOR (3 downto 0); --anode outputs selecting individual displays 3 to 0
           seg : out STD_LOGIC_VECTOR (0 to 6); -- cathode outputs for selecting LED-s in each display
           dp_in : in STD_LOGIC_VECTOR (3 downto 0); --decimal point input values
           dp_out : out STD_LOGIC; --selected decimal point sent to cathodes
           rst : in STD_LOGIC); --global reset
end component driver7seg;

begin

u_ssd: driver7seg port map ( clk => clk,
               Din => Din,
               an => an,
               seg => seg,
               dp_in => dp_in,
               dp_out => dp,
               rst => rst);

process (clk, rst)
begin
  if rst = '1' then
    current_state <= start;
  elsif rising_edge(clk) then
    current_state <= next_state;
  end if;    
end process;
        
process (current_state, currentDice, btnC, rerollCurrent)
variable swSum: integer;
variable localRerollTarget: integer;
variable localDiceToReroll: int_array;
variable goToScoreCalculation: std_logic;

begin
  swSum := 0;
  localRerollTarget := 0;
  case current_state is
    when start => 
        -- init values
        player_change <= '0';
        next_state <= get_rnd;
        led <= ('1','1','0','0','0','0','0','0','0','0','0','0','0','0','0','0');
        goToScoreCalculation := '0';
    when get_rnd => 
        -- keep state in get_rnd until 4 dice are generated
        if currentDice < 4 then
            next_state <= get_rnd;
        else
            next_state <= show;
        end if;
        led <= ('1','1','1','1','0','0','0','0','0','0','0','0','0','0','0','0');
    when show => 
        -- after reroll results are shown go to sum calculation
        if goToScoreCalculation = '0' then
            next_state <= user_select;
        else
            next_state <= dice_value_calculation;
        end if;
        led <= ('1','1','1','1','1','1','0','0','0','0','0','0','0','0','0','0');
    when user_select =>
        -- sum active swiches (reroll values)
        if sw(15) = '1' then
            swSum := swSum + 1;
        end if;
        if sw(14) = '1' then
            swSum := swSum + 1;
        end if;
        if sw(13) = '1' then
            swSum := swSum + 1;
        end if;
        if sw(12) = '1' then
            swSum := swSum + 1;
        end if;
        -- if select button is pressed and the number of selected values are correct select values to reroll
        if (btnC = '1') and (swSum < 3) then 
            if sw(15) = '1' then
                localDiceToReroll(localRerollTarget) := 0;
                localRerollTarget := localRerollTarget + 1;
            end if;
            if sw(14) = '1' then
                localDiceToReroll(localRerollTarget) := 1;
                localRerollTarget := localRerollTarget + 1;
            end if;
            if sw(13) = '1' then
                localDiceToReroll(localRerollTarget) := 2;
                localRerollTarget := localRerollTarget + 1;
            end if;
            if sw(12) = '1' then
                localDiceToReroll(localRerollTarget) := 3;
                localRerollTarget := localRerollTarget + 1;
            end if;
            -- update global signal from local and go to reroll state 
            rerollTarget <= localRerollTarget;
            diceToReroll <= localDiceToReroll;
            next_state <= reroll;
        end if;
        led <= ('1','1','1','1','1','1','1','1','0','0','0','0','0','0','0','0');
    when reroll => 
        -- stay on reroll if the number of rerolled dice isn't correct
        if rerollCurrent < rerollTarget then 
            next_state <= reroll;
        else 
            goToScoreCalculation := '1';
            next_state <= show;
        end if;
        led <= ('1','1','1','1','1','1','1','1','1','1','0','0','0','0','0','0');
    when dice_value_calculation => 
        if rising_edge(clk) then 
            -- sum dice for the current player
            userDiceSum(userId) <= diceValues(0) + diceValues(1) + diceValues(2) + diceValues(3); 
            goToScoreCalculation := '0';
            -- if the current player is the last one go to score calculation
            -- else start the turn of the following player
            if userId < 3 then
                userId <= userId + 1;
                rerollTarget <= 0;
            
                player_change <= '1';
                next_state <= start;
            else
                next_state <= score_calculation;
            end if;
        end if;
        led <= ('1','1','1','1','1','1','1','1','1','1','1','1','0','0','0','0');
    when score_calculation => 
        -- reset user count
        userId <= 0;
        next_state <= show_score;
        led <= ('1','1','1','1','1','1','1','1','1','1','1','1','1','1','0','0');
    when show_score => 
        -- stay on show score until the accept button is pressed
        if btnC = '1' then
            next_state <= start;
        else 
            next_state <= show_score;
        end if;
        led <= ('1','1','1','1','1','1','1','1','1','1','1','1','1','1','1','1');
    when others => next_state <= start;
  end case;                                            
end process;

--random number generator
lfsr: process (clk, rst)
variable shiftreg : std_logic_vector(15 downto 0) := x"ABCD";
variable firstbit : std_logic;
begin
  if rst = '1' then
    shiftreg := x"ABCD";
    rnd <= "000";
  elsif rising_edge(clk) then
    firstbit := shiftreg(1) xnor  shiftreg(0);
    shiftreg := firstbit & shiftreg(15 downto 1);
    rnd <= shiftreg(2 downto 0);
  end if;
end process;

generate_i: process (clk, rst, player_change)
begin

  if rst = '1' then
     diceValues(currentDice) <= 0;
     currentDice <= 0;
  elsif player_change = '1' then
    -- if player change then reset counters (this happens here because of a limitation)
    currentDice <= 0;
    rerollCurrent <= 0;
  elsif rising_edge(clk) then
     if (current_state = get_rnd) and (currentDice < 4) then
        diceValues(currentDice) <= to_integer(unsigned(rnd)) + 1;
        currentDice <= currentDice + 1;
     elsif current_state = reroll and rerollCurrent < rerollTarget then
        diceValues(diceToReroll(rerollCurrent)) <= to_integer(unsigned(rnd)) + 1;
        rerollCurrent <= rerollCurrent + 1;
     end if;
  end if;
end process;

show_random_values: process (clk, rst)
begin
    if current_state = show then    
        Din <= std_logic_vector(to_unsigned(diceValues(0), 4)) & 
               std_logic_vector(to_unsigned(diceValues(1), 4)) &
               std_logic_vector(to_unsigned(diceValues(2), 4)) &
               std_logic_vector(to_unsigned(diceValues(3), 4));
    elsif current_state = show_score then
        Din <= std_logic_vector(to_unsigned(userScores(0), 4)) & 
               std_logic_vector(to_unsigned(userScores(1), 4)) &
               std_logic_vector(to_unsigned(userScores(2), 4)) &
               std_logic_vector(to_unsigned(userScores(3), 4));
    end if;
end process;

calculate_score: process(clk, rst)
variable maxValue: integer;
variable maxIndex: integer;
begin
    maxValue := 0;
    maxIndex := 0;
    if current_state = score_calculation and rising_edge(clk) then
        for index in 0 to 3 loop
            if userDiceSum(index) > maxValue then
                maxValue := userDiceSum(index);
                maxIndex := index;
            end if;
        end loop;
        userScores(maxIndex) <= userScores(maxIndex) + 1;
     end if;
        
end process;

end Behavioral;

------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- FIFO
--Library UNISIM;
--use UNISIM.vcomponents.all;
LIBRARY XilinxCoreLib;

library work;
--use work.utilities_pkg.all;
use work.custom_common_pkg.all;
use work.genram_pkg.all;

------------------------------------------------------------------------------
-- Entity section
------------------------------------------------------------------------------

entity udp_if is
  generic
  (
    -- Three 32-bit data input. LSB bits are valid.
    G_NBITS_VALID_INPUT   : natural := 128;
    G_NBITS_DATA_INPUT    : natural := 128;
    G_OVF_COUNTER_SIZE    : natural := 10
  );
  port
  (
    -- External Ports. S2MM (streaming to memory mapped)
    udp_clk_i           : in  std_logic;
    udp_valid_o         : out std_logic;
    udp_data_o          : out std_logic_vector(G_NBITS_DATA_INPUT-1 downto 0);
    udp_be_o            : out std_logic_vector(G_NBITS_DATA_INPUT/8 - 1 downto 0);
    udp_last_o          : out std_logic;
    udp_ready_i         : in  std_logic;
    
    -- From ADC
    data_clk_i          : in  std_logic;
    data_i              : in  std_logic_vector(G_NBITS_DATA_INPUT-1 downto 0);
    data_valid_i        : in  std_logic;
    data_ready_o        : out std_logic;
    
    -- Capture control
    capture_ctl_i       : in  std_logic_vector(31 downto 0);
    send_ctl_cnt_o      : out std_logic_vector(44 downto 0);
    udp_complete_o      : out std_logic;
    udp_ovf_o           : out std_logic;
    
    -- Reset signal
    rst_i               : in  std_logic;
    
    -- Debug Signals
    udp_debug_clk_o     : out std_logic;
    udp_debug_data_o    : out std_logic_vector(255 downto 0);
    udp_debug_trigger_o : out std_logic_vector(15 downto 0)
  );
end entity udp_if;

architecture IMP of udp_if is

-- fifo component
-----------------

--  component fifo is
--  port (
--    rst       : in  std_logic;
--    wr_clk    : in  std_logic;
--    rd_clk    : in  std_logic;
--    wr_en     : in  std_logic;
--    rd_en     : in  std_logic;
--    din       : in  std_logic_vector(32 downto 0);
--    dout      : out std_logic_vector(32 downto 0);
--    full      : out std_logic;
--    empty     : out std_logic;
--    rd_data_count   : out std_logic_vector (12 downto 0); 
--    wr_data_count   : out std_logic_vector (12 downto 0) 
--  );
--  end component fifo;



  constant C_DATA_SIZE  : natural := 32;
  --constant G_OVF_COUNTER_SIZE   : natural := 10;
  -- FIFO signals index
  constant C_X_DATA     : natural := 3;
  constant C_Y_DATA     : natural := 2;
  constant C_Z_DATA     : natural := 1;
  constant C_W_DATA     : natural := 0;

  ------------------------------------------
  -- FIFO Signals
  ------------------------------------------
  type fifo_data    is array(0 to (G_NBITS_VALID_INPUT/C_DATA_SIZE)-1) of std_logic_vector(32 downto 0);
  type fifo_count   is array(0 to (G_NBITS_VALID_INPUT/C_DATA_SIZE)-1) of std_logic_vector(12 downto 0);
  type fifo_parity  is array(0 to (G_NBITS_VALID_INPUT/C_DATA_SIZE)-1) of std_logic_vector(7 downto 0);
  type fifo_ctrl    is array(0 to (G_NBITS_VALID_INPUT/C_DATA_SIZE)-1) of std_logic;

  signal fifo_do_concat     : std_logic_vector(G_NBITS_VALID_INPUT-1 downto 0);
  signal data_i_d1          : std_logic_vector(G_NBITS_DATA_INPUT-1 downto 0);

  -- read data_i: 64-bit (each) output: read output data_i
  signal fifo_do            : fifo_data;
  -- status: 1-bit (each) output: flags and other fifo status outputs
  signal fifo_empty         : fifo_ctrl;
  signal fifo_full          : fifo_ctrl;
  -- read control signals: 1-bit (each) input: read clock, enable and reset input signals
  signal fifo_rdclk         : fifo_ctrl;
  signal fifo_rden          : fifo_ctrl;
  signal fifo_rstn          : fifo_ctrl;
  signal fifo_rst           : fifo_ctrl;
  -- counter fifo signals
  signal fifo_rd_data_count   : fifo_count;
  signal fifo_wr_data_count   : fifo_count;
  -- write control signals: 1-bit (each) input: write clock and enable input signals
  signal fifo_wrclk           : fifo_ctrl;
  signal fifo_wren            : fifo_ctrl;
  -- write data_i: 64-bit (each) input: write input data_i
  signal fifo_di              : fifo_data;
  signal last_data_reg        : std_logic;
  -- Overflow counter. One extra bit for overflow easy overflow detection
  signal s_fifo_ovf_c         : std_logic_vector(G_OVF_COUNTER_SIZE downto 0);
  signal s_fifo_ovf           : std_logic;

  ------------------------------------------
  -- Internal Control
  ------------------------------------------
  --signal capture_ctl_reg        : std_logic_vector(21 downto 0);
  signal capture_ctl_reg        : std_logic_vector(31 downto 0);
  signal start_acq              : std_logic;
  signal start_acq_reg0         : std_logic;   
  signal start_acq_reg1         : std_logic; 
  signal start_acq_reg2         : std_logic; 
  signal start_acq_trig         : std_logic; 

  ------------------------------------------
  -- Reset Synch
  ------------------------------------------
  signal data_rst_reg0          : std_logic;
  signal data_rst_reg1          : std_logic;
  signal data_clk_rst           : std_logic;

  signal udp_rst_reg0           : std_logic;
  signal udp_rst_reg1           : std_logic;
  signal udp_clk_rst            : std_logic;

  ------------------------------------------
  -- UDP output signals
  ------------------------------------------
  -- G_NBITS_DATA_INPUT+1 bits. G_NBITS_DATA_INPUT bits (LSBs) for data_i and 1 bit (MSB) for last data_i bit
  signal udp_data_out0          : std_logic_vector(G_NBITS_DATA_INPUT downto 0);
  signal udp_valid_out0         : std_logic;

  signal udp_data_out1          : std_logic_vector(G_NBITS_DATA_INPUT downto 0);
  signal udp_valid_out1         : std_logic;

  signal udp_data_out2          : std_logic_vector(G_NBITS_DATA_INPUT downto 0);
  signal udp_valid_out2         : std_logic;

  signal udp_data_out3          : std_logic_vector(G_NBITS_DATA_INPUT downto 0);
  signal udp_valid_out3         : std_logic;

  signal udp_valid_s            : std_logic;
  signal udp_ready_s            : std_logic;
  signal udp_last_s             : std_logic;
  signal s_last_data            : std_logic;
  signal udp_valid_reg0         : std_logic;
  --signal udp_valid_reg1         : std_logic;
  
  -- Counter to coordinate the FIFO output - UDP input
  signal output_counter_rd        : std_logic_vector(1 downto 0);
  signal pre_output_counter_wr    : std_logic_vector(1 downto 0);
  
  -- Glue signals
  signal s_udp_complete           : std_logic;
  signal s_udp_last_glue          : std_logic;
  signal s_udp_valid_glue         : std_logic;
  signal s_udp_data_glue          : std_logic_vector(G_NBITS_DATA_INPUT-1 downto 0);


  begin

  send_ctl_cnt_o(44 downto 32) <= fifo_wr_data_count(C_W_DATA);
  send_ctl_cnt_o(31 downto 0)  <= capture_ctl_reg;
  

  -- UDP signals glue
  udp_last_o                        <= s_udp_last_glue;
  udp_valid_o                       <= s_udp_valid_glue;
  udp_data_o                        <= s_udp_data_glue;
  
  -- Debug data_i
  udp_debug_clk_o                   <= udp_clk_i;
  
  udp_debug_trigger_o(15 downto 6)  <= (others => '0');
  udp_debug_trigger_o(5)            <= fifo_full(C_W_DATA);
  udp_debug_trigger_o(4)            <= start_acq_trig;
  udp_debug_trigger_o(3)            <= capture_ctl_reg(31);
  udp_debug_trigger_o(2)            <= udp_ready_i;
  udp_debug_trigger_o(1)            <= s_udp_last_glue;
  udp_debug_trigger_o(0)            <= s_udp_valid_glue;
  
  udp_debug_data_o(255 downto 120)  <= (others => '0');
  udp_debug_data_o(119 downto 109)  <= s_fifo_ovf_c(10 downto 0);
  udp_debug_data_o(108)             <= s_udp_complete;
  udp_debug_data_o(107)             <= start_acq_trig;
  udp_debug_data_o(106)             <= fifo_full(C_W_DATA);
  udp_debug_data_o(105 downto 84)   <= capture_ctl_reg(21 downto 0);
  udp_debug_data_o(83 downto 52)    <= s_udp_data_glue(31 downto 0);
  udp_debug_data_o(51 downto 36)    <= fifo_do(C_W_DATA)(15 downto 0);-- FIXXXX
  udp_debug_data_o(35 downto 34)    <= output_counter_rd;
  udp_debug_data_o(33 downto 32)    <= pre_output_counter_wr;
  udp_debug_data_o(31 downto 19)    <= fifo_wr_data_count(C_W_DATA);--(5 downto 0);
  udp_debug_data_o(18 downto  6)    <= fifo_rd_data_count(C_W_DATA);--(5 downto 0);
  udp_debug_data_o(5)               <= udp_ready_s;
  udp_debug_data_o(4)               <= udp_valid_reg0;
  udp_debug_data_o(3)               <= udp_valid_s;
  udp_debug_data_o(2)               <= udp_ready_i;
  udp_debug_data_o(1)               <= s_udp_last_glue;
  udp_debug_data_o(0)               <= s_udp_valid_glue;
  
  --------------------------------
  -- Reset Logic
  --------------------------------
  -- FIFO reset cycle:  RST must be held high for at least three RDCLK clock cycles,
  --	and RDEN must be low for four clock cycles before RST becomes active high, and RDEN 
  -- remains low during this reset cycle.
  -- Is this really necessary? REVIEW!

  -- Guarantees the synchronicity with the input clock on reset deassertion
  cmp_reset_synch_udp : reset_synch
  port map
  (
    clk_i         => udp_clk_i,
    asyncrst_i    => rst_i,
    rst_o         => udp_clk_rst
  );

  cmp_reset_synch_data : reset_synch
  port map
  (
    clk_i         => data_clk_i,
    asyncrst_i    => rst_i,
    rst_o         => data_clk_rst
  );

  --------------------------------
  -- Start Acquisition logic
  --------------------------------
  -- Simple trigger detector 0 -> 1 for start_acq.
  -- Synchronize with bus clock data_clk_i might not be the same
  p_start_acq_trig : process (data_clk_i)
  begin
  if rising_edge(data_clk_i) then
    if data_clk_rst = '1' then
      start_acq_reg0 <= '0';
      start_acq_reg1 <= '0';
      start_acq_reg2 <= '0';
      start_acq_trig <= '0';
    else
      -- More flip flop levels than necessary because bus_clk and data_clk_i might be different!
      start_acq_reg0 <= start_acq;
      start_acq_reg1 <= start_acq_reg0;
      start_acq_reg2 <= start_acq_reg1;
      start_acq_trig <= (not start_acq_reg2) and start_acq_reg1;
      --start_acq_trig <= start_acq_reg2 xor start_acq_reg1;
    end if;
  end if;
   end process p_start_acq_trig;

   -- Bit representing the start acquisition signal
  start_acq <= capture_ctl_i(31);
  
  --------------------------------
  -- Samples Counter Logic
  --------------------------------
  -- Hold counter for "capture_count" clock cycles
  p_samples_counter : process (data_clk_i)
  begin
  if rising_edge(data_clk_i) then
    if data_clk_rst = '1' then
      capture_ctl_reg <= (others => '0');
    elsif capture_ctl_reg(31) = '1' and data_valid_i = '1' and fifo_full(C_W_DATA) = '0' then -- start counting and stop only when we have input all data to fifos
      capture_ctl_reg <= std_logic_vector(unsigned(capture_ctl_reg) - 1);
    elsif start_acq_trig = '1' then   -- assign only when 0 -> 1 transition of MSB of start_acq. MSB of capture_ctl_reg
      if (data_valid_i = '1') then
        --capture_ctl_reg <= '1' & std_logic_vector(unsigned(capture_ctl_i(20 downto 0)) - 1);  -- MSB of capture_ctl_i might not be 1 by this time. Force to 1 then...
        capture_ctl_reg <= std_logic_vector(unsigned('1' & capture_ctl_i(30 downto 0)) - 1);  -- MSB of capture_ctl_i might not be 1 by this time. Force to 1 then...
      else		-- Do not decrement now. wait until data_valid is set
        --capture_ctl_reg <= '1' & std_logic_vector(unsigned(capture_ctl_i(20 downto 0)));
        capture_ctl_reg <= std_logic_vector(unsigned('1' & capture_ctl_i(30 downto 0)));
      end if;
    end if;
  end if;
  end process p_samples_counter;

  --------------------------------
  -- UDP Last Data Logic
  --------------------------------

  p_last_data_proc : process(data_clk_i)
  begin
    if rising_edge(data_clk_i) then
      if data_clk_rst = '1' then
        last_data_reg <= '0';
      --elsif s_last_data = '1' then
      --	last_data_reg <= data_valid_i;
      --else 
      --	last_data_reg <= '0';
      else
        last_data_reg <= s_last_data;
      end if;
    end if;
  end process p_last_data_proc;
  
  -- bit 21 = 1 and bits 20 downto 0 = 0
  s_last_data   <= '1' when capture_ctl_reg(31 downto 0) = x"80000000" and data_valid_i = '1' else '0';

  --------------------------------
  -- FIFO Write Enable Logic
  --------------------------------
  
  gen_fifo_wren_inst : for i in 0 to (G_NBITS_VALID_INPUT/C_DATA_SIZE)-1 generate
  p_fifo_wr_en : process(data_clk_i)
  begin
  if rising_edge(data_clk_i) then
    if data_clk_rst = '1' then
      --last_data_s <= '0';
      --gen_fifo_signals_inst : for i in 0 to (G_NBITS_VALID_INPUT/C_DATA_SIZE)-1 generate
        fifo_wren(i) <= '0';
      --end generate;
    -- We only need to consider one as all FIFOs are synchronized with each other
    elsif fifo_full(C_W_DATA) = '0' then
      -- input data to fifo only when data is valid
      fifo_wren(i) <= data_valid_i and capture_ctl_reg(31);
    end if;
    
    --Necessary in order to input data to FIFO correctly as fifo_wren is registered
    if capture_ctl_reg(31) = '1' then
      data_i_d1 <= data_i;
    end if;

  end if;
  end process p_fifo_wr_en;
  end generate;
  
  --------------------------------
  -- UDP Output Logic
  --------------------------------
  udp_ready_s <= udp_ready_i or not s_udp_valid_glue;
  -- fifo is not empty and udp is ready
  udp_valid_s <= '0' when fifo_empty(C_W_DATA) = '1' else udp_ready_i;
  
  -- FIFO concatenation
  gen_fifo_do_concat_inst : for i in 0 to (G_NBITS_VALID_INPUT/C_DATA_SIZE)-1 generate
    fifo_do_concat(C_DATA_SIZE*(i+1)-1 downto C_DATA_SIZE*i)  <=  fifo_do(i)(C_DATA_SIZE-1 downto 0);
  end generate;

  -- We have a 2 output delay for FIFO. That being said, if we have a udp_ready_i signal it will take 2 udp clock cycles
  -- in order to read the data_i from FIFO.
  -- By this time, udp_ready_i might not be set and we have to wait for it. To solve this 2 delay read cycle
  -- it is employed a small 4 position "buffer" to hold the values read from fifo but not yet passed to the udp.
  -- Note that udp_valid_reg0 is 1 clock cycle delayed in relation to udp_valid_s. That should give time to
  -- FIFO output the data_i requested. Also not that that difference between pre_output_counter_wr and output_counter_rd
  -- is at most (at any given point in time) not greater than 2. Thus, with a 2 bit counter, we will not have overflow
  p_udp_pre_output : process(udp_clk_i)
  begin
  if rising_edge(udp_clk_i) then
    if udp_clk_rst = '1' then
      udp_data_out0 <= (others => '0');
      udp_valid_out0 <= '0';
      udp_data_out1 <= (others => '0');
      udp_valid_out1 <= '0';
      udp_data_out2 <= (others => '0');
      udp_valid_out2 <= '0';
      udp_data_out3 <= (others => '0');
      udp_valid_out3 <= '0';
      
      udp_valid_reg0 <= '0';
      --udp_valid_reg1 <= '0';
      pre_output_counter_wr <= (others => '0');
    -- fifo is not empty and udp is ready
    else--if udp_valid_reg1 = '1' then -- fifo output should be valid by now as fifo_rden was enabled and it id not empty!
      -- Store output from FIFO in the correct udp_data_outX if udp_valid_reg1 is valid.
      -- On the next udp_valid_reg1 operation (next clock cycle if udp_valid_reg1 remains 1),
      -- clear the past udp_data_outX if udp has read from it (read pointer is in the past write position).
      if  pre_output_counter_wr = "00" and udp_valid_reg0 = '1' then
        -- Output only the last_data bit of C_X_DATA as all the others are equal
        udp_data_out0(G_NBITS_DATA_INPUT) <= fifo_do(C_W_DATA)(C_DATA_SIZE);
        -- Output the data from fifo itself
        udp_data_out0(G_NBITS_DATA_INPUT-1 downto 0) <= std_logic_vector(RESIZE(unsigned(fifo_do_concat), G_NBITS_DATA_INPUT));
        udp_valid_out0 <= '1';
      elsif output_counter_rd = "00" and udp_ready_s = '1' then
        udp_data_out0 <= (others => '0');
        udp_valid_out0 <= '0';
      end if;
      
      if  pre_output_counter_wr = "01" and udp_valid_reg0 = '1' then --udp_valid_reg1 = '1' then
        udp_data_out1(G_NBITS_DATA_INPUT) <= fifo_do(C_W_DATA)(C_DATA_SIZE);
        udp_data_out1(G_NBITS_DATA_INPUT-1 downto 0) <= std_logic_vector(RESIZE(unsigned(fifo_do_concat), G_NBITS_DATA_INPUT));
        udp_valid_out1 <= '1';
      elsif output_counter_rd = "01" and udp_ready_s = '1' then
        udp_data_out1 <= (others => '0');
        udp_valid_out1 <= '0';
      end if;
      
      if  pre_output_counter_wr = "10" and udp_valid_reg0 = '1' then
        udp_data_out2(G_NBITS_DATA_INPUT) <= fifo_do(C_W_DATA)(C_DATA_SIZE);
        udp_data_out2(G_NBITS_DATA_INPUT-1 downto 0) <= std_logic_vector(RESIZE(unsigned(fifo_do_concat), G_NBITS_DATA_INPUT));
        udp_valid_out2 <= '1';
      elsif output_counter_rd = "10" and udp_ready_s = '1' then
        udp_data_out2 <= (others => '0');
        udp_valid_out2 <= '0';
      end if;
      
      if  pre_output_counter_wr = "11" and udp_valid_reg0 = '1' then
        udp_data_out3(G_NBITS_DATA_INPUT) <= fifo_do(C_W_DATA)(C_DATA_SIZE);
        udp_data_out3(G_NBITS_DATA_INPUT-1 downto 0) <= std_logic_vector(RESIZE(unsigned(fifo_do_concat), G_NBITS_DATA_INPUT));
        udp_valid_out3 <= '1';
      elsif output_counter_rd = "11" and udp_ready_s = '1' then
        udp_data_out3 <= (others => '0');
        udp_valid_out3 <= '0';
      end if;
      
      if udp_valid_reg0 = '1' then --udp_valid_reg0 = '1' then
        pre_output_counter_wr <= std_logic_vector(unsigned(pre_output_counter_wr) + 1);
      end if;
    
    -- 2 clock cycle delay for read from fifo.
    -- Nedded to break logic into one more FF as timing constraint wasn't met,
    -- due to the use of udp_valid_s directly into fifo_rden.
    -- This is not a problem since there is a 4 position "buffer" after this
    -- to absorb udp_ready_i deassertion
    udp_valid_reg0 <= udp_valid_s;
    --udp_valid_reg0 <= udp_valid_s;
    --udp_valid_reg1 <= udp_valid_reg0;
    end if;
  end if;
  end process p_udp_pre_output;
  
  -- Send to udp the correct data_i from udp_data_outW, based on the currently read pointer position
  p_udp_output_proc : process(udp_clk_i)
  begin
  if rising_edge(udp_clk_i) then
    if udp_clk_rst = '1' then
      s_udp_data_glue <= (others => '0');
      s_udp_valid_glue <= '0';
      udp_be_o <= (others => '0');
      -- The MSB is an indicator of the last data_i requested!
      s_udp_last_glue <= '0';
      output_counter_rd <= (others => '0');
    elsif udp_ready_s = '1' then
      -- verify wr counter and output corresponding output
      case output_counter_rd is
        when "11" =>
          s_udp_data_glue <= udp_data_out3(G_NBITS_DATA_INPUT-1 downto 0);
          s_udp_valid_glue <= udp_valid_out3;
          udp_be_o <= (others => '1');
          -- The MSB is an indicator of the last data_i requested!
          s_udp_last_glue <= udp_data_out3(G_NBITS_DATA_INPUT) and udp_valid_out3;	-- Error ?? CHECK!!!1
        when "10" =>
          s_udp_data_glue <= udp_data_out2(G_NBITS_DATA_INPUT-1 downto 0);
          s_udp_valid_glue <= udp_valid_out2;
          udp_be_o <= (others => '1');
          -- The MSB is an indicator of the last data_i requested!
          s_udp_last_glue <= udp_data_out2(G_NBITS_DATA_INPUT) and udp_valid_out2;
        when "01" =>
          s_udp_data_glue <= udp_data_out1(G_NBITS_DATA_INPUT-1 downto 0);
          s_udp_valid_glue <= udp_valid_out1;
          udp_be_o <= (others => '1');
          -- The MSB is an indicator of the last data_i requested!
          s_udp_last_glue <= udp_data_out1(G_NBITS_DATA_INPUT) and udp_valid_out1;
        --when "01" =>
        when others => 
          s_udp_data_glue <= udp_data_out0(G_NBITS_DATA_INPUT-1 downto 0);
          s_udp_valid_glue <= udp_valid_out0;
          udp_be_o <= (others => '1');
          -- The MSB is an indicator of the last data_i requested!
          s_udp_last_glue <= udp_data_out0(G_NBITS_DATA_INPUT) and udp_valid_out0;
      end case;
      
      -- Only increment output_counter_rd if it is different from pre_output_counter_wr
      -- to prevent overflow!
      if output_counter_rd /= pre_output_counter_wr then
        output_counter_rd <= std_logic_vector(unsigned(output_counter_rd) + 1);
      end if;
    end if;
  end if;
  end process p_udp_output_proc;
  
  -- Simple backpressure scheme. Should be almost full for correct behavior.
  -- fifo_full is already synchronized with fifo write_clock
  --data_ready_o    <= ((not fifo_full(C_W_DATA)) and capture_ctl_reg(21))
  data_ready_o    <= ((not fifo_full(C_W_DATA)) and capture_ctl_reg(31)) or
                     (capture_ctl_reg(30) and capture_ctl_reg(29) and capture_ctl_reg(28) and
                      capture_ctl_reg(27) and capture_ctl_reg(26) and capture_ctl_reg(25) and
                      capture_ctl_reg(14) and capture_ctl_reg(23) and capture_ctl_reg(22) and
                      capture_ctl_reg(21) and capture_ctl_reg(20) and capture_ctl_reg(19) and
                      capture_ctl_reg(18) and capture_ctl_reg(17) and capture_ctl_reg(16) and
                      capture_ctl_reg(15) and capture_ctl_reg(14) and capture_ctl_reg(13) and
                      capture_ctl_reg(12) and capture_ctl_reg(11) and capture_ctl_reg(10) and
                      capture_ctl_reg(9)  and capture_ctl_reg(8)  and capture_ctl_reg(7)  and
                      capture_ctl_reg(6)  and capture_ctl_reg(5)  and capture_ctl_reg(4)  and
                      capture_ctl_reg(3)  and capture_ctl_reg(2)  and capture_ctl_reg(1)  and
                      capture_ctl_reg(0));
  
  --------------------------------
  -- udp complete status
  --------------------------------
  udp_last_s  <= s_udp_valid_glue and udp_ready_i and s_udp_last_glue;

  p_udp_complete : process (udp_clk_i)
  begin
  if rising_edge(udp_clk_i) then
    if udp_clk_rst = '1' then	
      s_udp_complete <= '0';
    elsif udp_last_s = '1' then
      -- udp could be held to 1 when completed, but it would be more difficult
      -- to bring it back to 0, since the udp transfer is initiated in the data_clk_i domain
      s_udp_complete <= not s_udp_complete;
    end if;
  end if;
  end process p_udp_complete;
  
  udp_complete_o  <=  s_udp_complete;
  
  --------------------------------
  -- udp overflow (fifo full) status and counter
  --------------------------------
  
  -- Data is lost when this is asserted.
  -- FIFO is full, there is data valid on input and we are in the middle of a udp transfer
  s_fifo_ovf    <= fifo_full(C_W_DATA) and data_valid_i and capture_ctl_reg(31);
  
  p_udp_overflow : process (data_clk_i)
  begin
  if rising_edge(data_clk_i) then
    --No need for reset. On configuration it will default to zero!
    if start_acq_trig = '1' then
      s_fifo_ovf_c <= (others => '0');
    elsif s_fifo_ovf = '1' then
      -- Even if the counter wrapps around, an overflow will still be detected!
      s_fifo_ovf_c <= '1' & std_logic_vector(unsigned(s_fifo_ovf_c(G_OVF_COUNTER_SIZE-1 downto 0)) + 1);
    end if;
  end if;
  end process p_udp_overflow;
  
  udp_ovf_o   <= s_fifo_ovf_c(G_OVF_COUNTER_SIZE);
  
  --------------------------------
  -- FIFO instantiation
  --------------------------------
  -- Indexes
  -- constant C_X_DATA      : natural := 3;
  -- constant C_Y_DATA      : natural := 2;
  -- constant C_Z_DATA      : natural := 1;
  -- constant C_W_DATA      : natural := 0;

  gen_fifo_inst : for i in 0 to (G_NBITS_VALID_INPUT/C_DATA_SIZE)-1 generate


--  cmp_fifo : fifo
--  port map(
--    rst             =>  fifo_rst(i),
--    wr_clk          =>  fifo_wrclk(i),
--    din             =>  fifo_di(i),
--    wr_en           =>  fifo_wren(i),
--    full            =>  fifo_full(i),
--    wr_data_count   =>  fifo_wr_data_count(i),
--    rd_clk          =>  fifo_rdclk(i),
--    dout            =>  fifo_do(i),
--    rd_en           =>  fifo_rden(i),
--    empty           =>  fifo_empty(i),
--    rd_data_count   =>  fifo_rd_data_count(i)
--  );


  cmp_fifo : generic_async_fifo
  generic map(
    g_data_width => G_NBITS_VALID_INPUT+1,
    g_size       => 8192,

    g_with_rd_count => true,  -- with words counter
    g_with_wr_count => true,

    g_almost_empty_threshold => 8192/5,
    g_almost_full_threshold  => (8192-(8192/5))

  )
  port map(
    rst_n_i  => fifo_rstn(i),

    -- write port
    clk_wr_i => fifo_wrclk(i),
    d_i      => fifo_di(i),
    we_i     => fifo_wren(i),

    wr_empty_o        => open,
    wr_full_o         => fifo_full(i),
    wr_almost_empty_o => open,
    wr_almost_full_o  => open,
    wr_count_o        => fifo_wr_data_count(i),

    -- read port
    clk_rd_i => fifo_rdclk(i),
    q_o      => fifo_do(i),
    rd_i     => fifo_rden(i),

    rd_empty_o        => fifo_empty(i),
    rd_full_o         => open,
    rd_almost_empty_o => open,
    rd_almost_full_o  => open,
    rd_count_o        => fifo_rd_data_count(i)
    );



  end generate gen_fifo_inst;

  gen_fifo_signals_inst : for i in 0 to (G_NBITS_VALID_INPUT/C_DATA_SIZE)-1 generate
    -- Drive signals for FIFO. Do a RESET CYCLE! Watch for constraints
    fifo_rstn(i)      <= not udp_clk_rst;
    fifo_rst(i)       <= udp_clk_rst;
    fifo_rden(i)      <= udp_valid_s; 
    fifo_rdclk(i)     <= udp_clk_i;
    -- Observe the FIFO reset cycle! udp_clk_buf is the clock for fifo_rd_en
    fifo_wrclk(i)     <= data_clk_i;
    -- C_DATA_SIZE + 1 bits.
    -- It doesn't matter if the data_i is signed or unsigned since we do not care what the input data is.
    -- The user has to treat this and extend the sign if necessary.
    fifo_di(i)(C_DATA_SIZE downto 0)  <= last_data_reg & data_i_d1(C_DATA_SIZE*(i+1) - 1 downto C_DATA_SIZE*i);
  end generate;

  
end IMP;

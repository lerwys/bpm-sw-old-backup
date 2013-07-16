--------------------------------------------------------------------------------
-- Based on original UDP_integration_example, 2011
--------------------------------------------------------------------------------
--  __       __   __                         ___       _   _   __      _  .  _  
-- |   |\ | |__| |__ |\  /|  _  |   |\ | |   |__   _  | \ |_| |    _  | \ | | _ 
-- |__ | \| |    |__ | \/ |     |__ | \| |__ ___|     |_/ | | |__     |_/ | |_/ 
--                                                                              
--------------------------------------------------------------------------------
-- Title      : UPD Packet Generator
--------------------------------------------------------------------------------
-- Author     : José Alvim Berkenbrock
-- Company    : CNPEM LNLS-DAC-DIG
-- Platform   : FPGA-generic
--------------------------------------------------------------------------------
-- Description:  This design sweeps the vector data_array and sends its words to
--              fifo after receive a pulse in capture_ctl_i (31) _@chipscope. 
--              Saved on fifo, the block waits for pulse in button input, which 
--              initiates the UDP-IP sending process.
--------------------------------------------------------------------------------
-- Copyright (c) 2013 CNPEM
-- Licensed under GNU Lesser General Public License (LGPL) v3.0
--------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author                Description
-- 2013-06-06  1.0      jose.berkenbrock      Created
-- 2013-06-28  2.0      jose.berkenbrock      UDP IF added
-- 2013-07-07  3.0      jose.berkenbrock      8-bits adaptation
--------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use work.axi.all;
use work.ipv4_types.all;
use work.arp_types.all;

entity udp_pkt_gen is
  generic (
    G_SYNT_OR_SIM       : boolean := false; -- when SIMULATING, set as FALSE
    
    G_NBITS_DATA_INPUT  : natural := 32; --192;   -- 24 bits for each X, Y, Q, SUM, Va, Vb, Vc, Vd values
    G_NPKT_2ACQ         : integer := 15 --24 --192
  );

  port (
    -- System signals
    ------------------
    reset                : in  std_logic;      -- asynchronous reset
    clk_in_p             : in  std_logic;      -- 200MHz clock input from board
    clk_in_n             : in  std_logic;

    --clk200_in             : in  std_logic;      -- 200MHz clock input
    --clk125_in             : in  std_logic;      -- 125MHz clock input

    button               : in  std_logic;
    display              : out std_logic_vector(7 downto 0);

    button_led           : out  std_logic;
    button_stb10         : out  std_logic;

----  udp_tx_data_out_ready_i   : in  std_logic;                                         -- just for simulation
----  capture_ctl_i             : in std_logic_vector(31 downto 0);                      -- just for simulation
----  data_sim_i                : in  std_logic_vector(G_NBITS_DATA_INPUT-1 downto 0);   -- just for simulation
----  data_valid_sim_i          : in  std_logic;                                         -- just for simulation


    -- GMII Interface
    -----------------
    phy_resetn           : out std_logic;
    gmii_txd             : out std_logic_vector(7 downto 0);
    gmii_tx_en           : out std_logic;
    gmii_tx_er           : out std_logic;
    gmii_tx_clk          : out std_logic;
    gmii_rxd             : in  std_logic_vector(7 downto 0);
    gmii_rx_dv           : in  std_logic;
    gmii_rx_er           : in  std_logic;
    gmii_rx_clk          : in  std_logic
  );
end udp_pkt_gen;

architecture rtl of udp_pkt_gen is


  ------------------------------------------------------------------------------
  -- Component Declaration for the Complete UDP Layer
  ------------------------------------------------------------------------------
  component UDP_Complete
    generic (
      CLOCK_FREQ      : integer := 125000000;   -- freq of data_in_clk -- needed to timout cntr
      ARP_TIMEOUT     : integer := 60;          -- ARP response timeout (s)
      ARP_MAX_PKT_TMO : integer := 5;           -- # wrong nwk pkts received before set error
      MAX_ARP_ENTRIES : integer := 255          -- max entries in the ARP store
    );
    port (
      -- UDP TX signals
      udp_tx_start          : in  std_logic;    -- indicates req to tx UDP
      udp_txi               : in  udp_tx_type;  -- UDP tx cxns
      udp_tx_result         : out std_logic_vector (1 downto 0);-- tx status (changes during transmission)
      udp_tx_data_out_ready : out std_logic;    -- indicates udp_tx is ready to take data
      -- UDP RX signals
      udp_rx_start          : out std_logic;    -- indicates receipt of udp header
      udp_rxo               : out udp_rx_type;
      -- IP RX signals
      ip_rx_hdr             : out ipv4_rx_header_type;
      -- system signals
      clk_in_p              : in  std_logic;    -- 200MHz clock input from board
      clk_in_n              : in  std_logic;
      --clk200_in             : in  std_logic;      -- 200MHz clock input
      --clk125_in             : in  std_logic;      -- 125MHz clock input
      clk_out               : out std_logic;      -- 125MHz clock output
      reset                 : in  std_logic;
      our_ip_address        : in  std_logic_vector(31 downto 0);
      our_mac_address       : in  std_logic_vector(47 downto 0);
      control               : in  udp_control_type;
      -- status signals
      arp_pkt_count         : out std_logic_vector (7 downto 0);   -- count of arp pkts received
      ip_pkt_count          : out std_logic_vector (7 downto 0);   -- number of IP pkts received for us
      -- GMII Interface
      phy_resetn            : out std_logic;
      gmii_txd              : out std_logic_vector (7 downto 0);
      gmii_tx_en            : out std_logic;
      gmii_tx_er            : out std_logic;
      gmii_tx_clk           : out std_logic;
      gmii_rxd              : in  std_logic_vector (7 downto 0);
      gmii_rx_dv            : in  std_logic;
      gmii_rx_er            : in  std_logic;
      gmii_rx_clk           : in  std_logic;
      --
      stb10_out             : out std_logic;      -- 10Hz clock output
      locked_mmcm_o         : out std_logic;
      -- Debug Test LED
      LEDs                  : out std_logic_vector (2 downto 0)
    );
  end component;


  ------------------------------------------------------------------------------
  -- Component Declaration for the UDP Interface
  ------------------------------------------------------------------------------
  component udp_if
    generic(
      G_NBITS_VALID_INPUT   : natural;
      G_NBITS_DATA_INPUT    : natural;
      G_OVF_COUNTER_SIZE    : natural
    );
    port(
      -- External Ports. S2MM (streaming to memory mapped)
      udp_clk_i             : in  std_logic;
      udp_valid_o           : out std_logic;
      udp_data_o            : out std_logic_vector(G_NBITS_DATA_INPUT-1 downto 0);
      udp_be_o              : out std_logic_vector(G_NBITS_DATA_INPUT/8 - 1 downto 0);
      udp_last_o            : out std_logic;
      udp_ready_i           : in  std_logic;
      
      -- From ADC
      data_clk_i            : in  std_logic;
      data_i                : in  std_logic_vector(G_NBITS_DATA_INPUT-1 downto 0);
      data_valid_i          : in  std_logic;
      data_ready_o          : out std_logic;
      
      -- Capture control
      capture_ctl_i         : in  std_logic_vector(31 downto 0);
      send_ctl_cnt_o        : out std_logic_vector(44 downto 0);
      udp_complete_o        : out std_logic;
      udp_ovf_o             : out std_logic;
      
      -- Reset signal
      rst_i                 : in std_logic;
      
      -- Debug Signals
      udp_debug_clk_o       : out std_logic;
      udp_debug_data_o      : out std_logic_vector(255 downto 0);
      udp_debug_trigger_o   : out std_logic_vector(15 downto 0)
    );
  end component;



  ----------------------------------------------------------------------------
  -- Component Declaration for the ChipScope VIO, ILA and ICON              --
  ----------------------------------------------------------------------------
  -- ICON                                                                   --
  component chipscope_icon_2_port                                            --
    port (                                                                  --
      CONTROL0  : inout std_logic_vector (35 downto 0);                     --
      CONTROL1  : inout std_logic_vector (35 downto 0));                    --
                                                                            --
  end component;                                                            --
                                                                            --
  -- VIO                                                                    --
  component chipscope_vio_1_port_256_bits                                   --
    port (                                                                  --
      CONTROL   : inout std_logic_vector (35 downto 0);                     --
      CLK       : in    std_logic;                                          --
      SYNC_OUT  : out   std_logic_vector(255 downto 0));                    --
                                                                            --
  end component;                                                            --
                                                                            --
  -- ILA                                                                    --
  component chipscope_ila_1port_1trg256b                                    --
    port (                                                                  --
      CONTROL   : inout std_logic_vector (35 downto 0);                     --
      CLK       : in    std_logic;                                          --
      TRIG0     : in    std_logic_vector(255 downto 0));                    --
                                                                            --
  end component;                                                            --
  ----------------------------------------------------------------------------



  type state_type is (IDLE, INIT_TRANS, DATA_OUT, PAUSE);
  type set_clr_type is (SET, CLR, HOLD);

  type my_array is array (0 to G_NPKT_2ACQ) of std_logic_vector(G_NBITS_DATA_INPUT-1 downto 0);
  signal data_array : my_array := (x"01020304", x"05060708", x"090A0B0C",
                                   x"0D0E0F10", x"11121314", x"15161718",
                                   x"191A1B1C", x"1D1E1F20", x"21222324",
                                   --x"25262728", x"292A2B2C", x"2D2E2F30",
                                   --x"31323334", x"35363738", x"393A3B3C",
                                   
                                   others => x"12345678" );
                                   --others => x"18" );


  -- system signals
  -----------------
  signal clk_int                    : std_logic;
  signal locked_mmcm                : std_logic;
  signal reset_int                  : std_logic;
  signal our_mac                    : std_logic_vector (47 downto 0);
  signal our_ip                     : std_logic_vector (31 downto 0);
    -- from ChipScope-vio
  signal cs_our_mac               : std_logic_vector (47 downto 0) := (others => '0');
  signal cs_our_ip                : std_logic_vector (31 downto 0) := (others => '0');
  signal cs_dst_mac               : std_logic_vector (47 downto 0) := (others => '0');
  signal cs_dst_ip                : std_logic_vector (31 downto 0) := (others => '0');
  
  signal udp_tx_int                 : udp_tx_type;
  signal udp_tx_result_int          : std_logic_vector (1 downto 0);
  signal udp_tx_data_out_ready_int  : std_logic;
  signal udp_rx_int                 : udp_rx_type;
  signal udp_tx_start_int           : std_logic;
  signal udp_rx_start_int           : std_logic;
  signal arp_pkt_count_int          : std_logic_vector(7 downto 0);
  signal ip_pkt_count_int           : std_logic_vector(7 downto 0);
  signal ip_rx_hdr_int              : ipv4_rx_header_type;


  -- state signals
  ----------------
  signal state              : state_type;
  signal tx_hdr             : udp_tx_header_type;
  signal tx_start_reg       : std_logic;
  signal tx_started_reg     : std_logic;
  signal tx_fin_reg         : std_logic;
  
  -- control signals
  ------------------
  signal next_state         : state_type;
  signal set_state          : std_logic;
  signal set_hdr            : std_logic;
  signal set_tx_start       : set_clr_type;
  signal set_last           : std_logic;
  signal set_tx_started     : set_clr_type;
  signal set_tx_fin         : set_clr_type;
  signal first_byte_rx      : std_logic_vector(7 downto 0);
  signal control_int        : udp_control_type;
  signal set_fifo_rden      : std_logic;
  
  
  -- udp interface signals
  ------------------------
  signal udp_valid          : std_logic;
  signal data2send          : std_logic_vector(7 downto 0);
  signal data_from_fifo     : std_logic_vector(G_NBITS_DATA_INPUT-1 downto 0);
  signal udp_be             : std_logic_vector(G_NBITS_DATA_INPUT/8 - 1 downto 0);
  signal udp_last           : std_logic;
  signal fifo_rden          : std_logic;

  signal data2save          : std_logic_vector(G_NBITS_DATA_INPUT-1 downto 0);
  signal data_valid         : std_logic;
  signal data_ready         : std_logic;

  signal capture_ctl        : std_logic_vector(31 downto 0);
  signal send_ctl_cnt       : std_logic_vector(44 downto 0);
  signal udp_if_complete    : std_logic;
  signal udp_ovf            : std_logic;

  signal udp_debug_data     : std_logic_vector(255 downto 0);
  signal udp_debug_trigger  : std_logic_vector(15 downto 0);
  signal udp_debug_clk      : std_logic;




  signal stb10              : std_logic;  -- from mac_v2_2
  signal cnt10_wr           : unsigned (31 downto 0);
  signal cnt10_pre_wr       : unsigned (31 downto 0);
  signal fifo_valid         : std_logic;
  signal fifo_valid_fw      : std_logic;
  signal fifo_first_data    : std_logic;


  -- other internal signal
  ------------------------
  signal LEDs_int           : std_logic_vector(2 downto 0); 
  signal state_status       : std_logic_vector(1 downto 0);
  signal button_rise_int    : std_logic;
  signal button_old_int     : std_logic;


  ----------------------------------------------------------------------------
  -- Chipscope Signals                                                      --
  --------------------                                                      --
  -- icon                                                                   --
  signal CONTROL0           : std_logic_vector (35 downto 0);               --
  signal CONTROL1           : std_logic_vector (35 downto 0);               --
  -- vio                                                                    --
  signal SYNC_OUT           : std_logic_vector(255 downto 0);               --
  -- ila                                                                    --
  signal TRIG0              : std_logic_vector(255 downto 0);               --
  ----------------------------------------------------------------------------



begin



  -- IP and MAC addr selection
  ----------------------------
  ip_addr_p : process (cs_our_ip, clk_int, our_ip)
  begin
    if rising_edge(clk_int) then
      if cs_our_ip = X"00000000" then
        our_ip <= x"0A0011AE";            -- 10.0.17.174
      else our_ip <= cs_our_ip;
      end if;
    else our_ip <= our_ip;
    end if;
  end process;

  mac_addr_p : process (cs_our_mac, clk_int, our_mac)
  begin
    if rising_edge(clk_int) then
      if cs_our_mac = X"000000000000" then 
        our_mac <= x"002320212223";
      else our_mac <= cs_our_mac;
      end if;
    else our_mac <= our_mac;
    end if;
  end process;

  control_int.ip_controls.arp_controls.clear_cache <= '0';
  ------------------------------------------------------------------------------


  -- ARP and IP Pkt received counter (none is expected)
  ----------------------------------
  display(3 downto 0) <= arp_pkt_count_int(3 downto 0);
  display(7 downto 4) <= ip_pkt_count_int(3 downto 0);


  p_pod_edge_button : process (clk_int)
  begin
  if (rising_edge(clk_int)) then
    if (reset = '1') then
    button_old_int <= '0';
    else
    button_old_int <= button;
    end if;
  end if;  
  end process;
  
  -- pos edge detection
  button_rise_int <= '1' when (button = '1') and (button_old_int = '0') else '0';


  button_led <= button;



      -- TX response process - COMB
  tx_proc_combinatorial: process(
    -- inputs
    udp_tx_data_out_ready_int,  udp_tx_int.data.data_out_valid,
    udp_tx_result_int, button_rise_int, data2send,
    -- state
    state, tx_hdr, tx_start_reg, tx_started_reg, tx_fin_reg, 
    -- controls
    next_state, set_state, set_hdr, set_tx_start, set_last, 
    set_tx_started, set_tx_fin, first_byte_rx, fifo_valid, send_ctl_cnt
      )
  begin
    -- set output_followers
    udp_tx_int.hdr                <= tx_hdr;
    udp_tx_int.data.data_out_last <= set_last;
    udp_tx_start_int              <= tx_start_reg;


    -- set control signal defaults
    next_state       <= IDLE;
    set_state        <= '0';
    set_hdr          <= '0';
    set_tx_start     <= HOLD;
    set_last         <= '0';
    set_tx_started   <= HOLD;
    set_tx_fin       <= HOLD;
    --set_fifo_rden  <= '0';
    fifo_rden        <= '0';
    first_byte_rx    <= (others => '0');
    udp_tx_int.data.data_out       <= (others => '0');
    udp_tx_int.data.data_out_valid <= '0';



    -- FSM
    case state is

      when IDLE =>
        --fifo_rden <= '0';

        udp_tx_int.data.data_out_valid <= '0';

        state_status <= "01";
        if button_rise_int = '1' then
          set_tx_fin <= CLR;
          set_hdr    <= '1';

          set_tx_started <= SET;
          set_tx_start   <= SET;
          --next_state     <= DATA_OUT;
          next_state     <= INIT_TRANS;
          set_state      <= '1';
        end if;

      -- Advance only when fifo has data.
      when INIT_TRANS =>
        if (data_ready = '1') then
          --set_fifo_rden  <= '1'; 
          set_state      <= '1';
          next_state     <= DATA_OUT;
          --next_state     <= WAIT_FIFO_DATA;
          state_status <= "10";
        end if;


      when DATA_OUT =>

        state_status <= "11";
        if udp_tx_result_int = UDPTX_RESULT_ERR then
          -- have an error from the IP TX layer, clear down the TX
          set_tx_start   <= CLR;
          set_tx_fin     <= SET;
          set_tx_started <= CLR;
          next_state     <= IDLE;
          set_state      <= '1';
        else
          if udp_tx_result_int = UDPTX_RESULT_SENDING then
            set_tx_start <= CLR;    -- reset out start req as soon as we know we are sending
          end if;


          udp_tx_int.data.data_out <= data2send;


          -- sink block will consume data in this cycle, so we read another 
          -- data from fifo to bee available on the next clock cycle.
          if udp_tx_data_out_ready_int = '1' and fifo_valid = '1' then
            fifo_rden <= '1';
          end if;


          udp_tx_int.data.data_out_valid <= fifo_valid;

          if ((udp_tx_data_out_ready_int = '1') and (fifo_valid = '1')) then
            if unsigned(send_ctl_cnt(44 downto 32)) = to_unsigned(0,13) then  --to_unsigned(3,13) then
              set_last       <= '1';
              set_tx_fin     <= SET;
              set_tx_started <= CLR;
              next_state     <= PAUSE;
              set_state      <= '1';
            end if;
          end if;

        end if;



      when PAUSE =>

        state_status <= "00";
          --set_last       <= '1';
          --set_tx_fin     <= SET;
          --set_tx_started <= CLR;
        next_state <= IDLE;
        set_state <= '1';

    end case;
  end process;



  p_fifo_first_data : process(clk_int)
  begin
    if rising_edge(clk_int) then
      if reset = '1' then
        fifo_first_data <= '0';

      elsif data_valid = '1' and send_ctl_cnt(31 downto 0) = x"80000000" then
        fifo_first_data <= '1';

      elsif fifo_rden = '1' then
        fifo_first_data <= '0';
      end if;
    end if;
  end process;

  fifo_valid <= fifo_valid_fw or fifo_first_data;

  p_fifo_valid_fw : process(clk_int)
  begin
    if rising_edge(clk_int) then
      if fifo_rden = '1' then
        fifo_valid_fw <= '1';

      elsif data_ready = '0' then
        fifo_valid_fw <= '0';
      end if;
    end if;
  end process;




  -- TX response process - SEQ
  tx_proc_sequential: process(clk_int)
  begin
  if rising_edge(clk_int) then
    if reset = '1' then
      -- reset state variables
      state              <= IDLE;
      tx_start_reg       <= '0';
      tx_hdr.dst_ip_addr <= (others => '0');
      tx_hdr.dst_port    <= (others => '0');
      tx_hdr.src_port    <= (others => '0');
      tx_hdr.data_length <= (others => '0');
      tx_hdr.checksum    <= (others => '0');
      tx_started_reg     <= '0';
      tx_fin_reg         <= '0';


    else

      -- Next rx_state processing
      if set_state = '1' then
        state <= next_state;
      else
        state <= state;
      end if;


      -- set tx hdr
      if set_hdr = '1' then
        if first_byte_rx = x"42" then
          tx_hdr.dst_ip_addr <= IP_BC_ADDR;       -- send to Broadcast addr -- ?????????????????????
        else
          tx_hdr.dst_ip_addr <= cs_dst_ip;
        end if;
        tx_hdr.dst_port    <= x"EE18";
        tx_hdr.src_port    <= x"EF05";
        tx_hdr.data_length <= std_logic_vector(to_unsigned(G_NPKT_2ACQ,16)); --x"0018";--x"0004";
        tx_hdr.checksum    <= x"0000";

      else
        tx_hdr <= tx_hdr;
      end if;

      -- set tx start signal
      case set_tx_start is
        when SET  => tx_start_reg <= '1';
        when CLR  => tx_start_reg <= '0';
        when HOLD => tx_start_reg <= tx_start_reg;
      end case;
    
      -- set tx started signal
      case set_tx_started is
        when SET  => tx_started_reg <= '1';
        when CLR  => tx_started_reg <= '0';
        when HOLD => tx_started_reg <= tx_started_reg;
      end case;
    
      -- set tx finished signal
      case set_tx_fin is
        when SET  => tx_fin_reg <= '1';
        when CLR  => tx_fin_reg <= '0';
        when HOLD => tx_fin_reg <= tx_fin_reg;
      end case;

    end if;
  end if;

  end process;


  -- Continuosly sweep data array
  fifo_wr : process (clk_int)
  begin
    if rising_edge(clk_int) then
      if reset_int = '1' then --reset = '1' then
        data_valid   <= '0';
        cnt10_wr     <= (others => '0');
        cnt10_pre_wr <= (others => '0');

      else
        if send_ctl_cnt(31) = '1' then
          data_valid <= stb10;

          if stb10 = '1' then
            cnt10_wr <= cnt10_pre_wr;
            -- this one cycle delay (register) was added to capture the first word of data array
            if (cnt10_pre_wr <= to_unsigned(G_NPKT_2ACQ, cnt10_pre_wr'length)) then
              cnt10_pre_wr <= cnt10_pre_wr + 1;

            end if;
          end if;

        else
          data_valid   <= '0';
          cnt10_wr     <= (others => '0');
          cnt10_pre_wr <= (others => '0');

        end if;
      end if;
    end if;
  end process;

  button_stb10 <= stb10;



  ------------------------------------------------------------------------------
  -- Instantiate the UDP layer
  ------------------------------------------------------------------------------
  UDP_block : UDP_Complete 
  generic map (
    ARP_TIMEOUT     => 10     -- timeout in seconds
  )
  PORT MAP (
    -- UDP interface
    udp_tx_start          => udp_tx_start_int,
    udp_txi               => udp_tx_int,
    udp_tx_result         => udp_tx_result_int,
    udp_tx_data_out_ready => udp_tx_data_out_ready_int, --open, when simulating  --udp_tx_data_out_ready_int when synt
    udp_rx_start          => udp_rx_start_int, --sig
    udp_rxo               => udp_rx_int,       --sig
    -- IP RX signals
    ip_rx_hdr             => ip_rx_hdr_int,--sig
    -- System interface
    clk_in_p              => clk_in_p,   --in
    clk_in_n              => clk_in_n,   --in
    clk_out               => clk_int,    --sig
    reset                 => reset,      --in
    our_ip_address        => our_ip,     --sig
    our_mac_address       => our_mac,    --sig
    control               => control_int,--sig
    -- status signals
    arp_pkt_count         => arp_pkt_count_int,--out
    ip_pkt_count          => ip_pkt_count_int, --out
    -- GMII Interface
    -----------------     
    phy_resetn            => phy_resetn, --out
    gmii_txd              => gmii_txd,   --out
    gmii_tx_en            => gmii_tx_en, --out
    gmii_tx_er            => gmii_tx_er, --out
    gmii_tx_clk           => gmii_tx_clk,--out
    gmii_rxd              => gmii_rxd,   --in
    gmii_rx_dv            => gmii_rx_dv, --in
    gmii_rx_er            => gmii_rx_er, --in
    gmii_rx_clk           => gmii_rx_clk,--in
    --
    stb10_out             => stb10,     --sig
    locked_mmcm_o         => locked_mmcm, --sig
    -- Debug Test LED
    LEDs                  => LEDs_int   --sig
  );




  data2save <= data_array(to_integer(cnt10_wr));


  ------------------------------------------------------------------------------
  -- Instantiate the UDP Interface
  ------------------------------------------------------------------------------
  
  udp_if_cmp: udp_if
  generic map (
    G_NBITS_VALID_INPUT => G_NBITS_DATA_INPUT,  -- ?
    G_NBITS_DATA_INPUT  => G_NBITS_DATA_INPUT,
    G_OVF_COUNTER_SIZE  => 10
  )
  port map (
    udp_clk_i           => clk_int,---
    udp_valid_o         => udp_valid,
    udp_data_o          => data_from_fifo,---
    udp_be_o            => udp_be,
    udp_last_o          => udp_last,
    udp_ready_i         => fifo_rden,---
  
    data_clk_i          => clk_int,---
    data_i              => data2save, -- data_sim_i, --
    data_valid_i        => data_valid, --data_valid_sim_i,--fifo_wren,---
    data_ready_o        => data_ready,
  
    capture_ctl_i       => capture_ctl,
    send_ctl_cnt_o      => send_ctl_cnt,--
    udp_complete_o      => udp_if_complete,--
    udp_ovf_o           => udp_ovf,
  
    rst_i               => reset,--reset_int,---
  
    udp_debug_clk_o     => udp_debug_clk,
    udp_debug_data_o    => udp_debug_data,
    udp_debug_trigger_o => udp_debug_trigger
  );

  data2send <= data_from_fifo(7 downto 0);

--  ----------------------------------------------------------------------------
--  gen_4sim : if (not G_SYNT_OR_SIM) generate -- just for simulation         --
--                                                                            --
--    udp_tx_data_out_ready_int <= udp_tx_data_out_ready_i;                   --
--                                                                            --
--    capture_ctl <= capture_ctl_i;                                           --
--                                                                            --
--  end generate gen_4sim;                                                    --
--  ----------------------------------------------------------------------------



  ----------------------------------------------------------------------------
  chipscope_4syn : if (G_SYNT_OR_SIM) generate                              --
  ----------------------------------------------------------------------------
  -- Instantiate the ICON Chipscope Component                               --
  ----------------------------------------------------------------------------
  icon_cmp : chipscope_icon_2_port                                           --
  port map (                                                                --
    CONTROL0 => CONTROL0,                                                   --
    CONTROL1 => CONTROL1);                                                  --
                                                                            --
  ----------------------------------------------------------------------------
  -- Instantiate the VIO Chipscope Component                                --
  ----------------------------------------------------------------------------
  vio_cmp : chipscope_vio_1_port_256_bits                                   --
  port map (                                                                --
    CONTROL  => CONTROL0,                                                   --
    CLK      => clk_int,                                                    --
    SYNC_OUT => SYNC_OUT);                                                  --
                                                                            --
  cs_our_mac(47 downto 0) <= (others => '0');  -- SYNC_OUT( 47 downto 0  ); --
  cs_our_ip (31 downto 0) <= (others => '0');  -- SYNC_OUT( 79 downto 48 ); --
  cs_dst_mac(47 downto 0) <= x"001F81000250";  -- SYNC_OUT(125 downto 78 ); --
  cs_dst_ip (31 downto 0) <= x"0A001186";      -- SYNC_OUT(157 downto 126); --
                                                                            --
  capture_ctl (31 downto 0) <= SYNC_OUT(189 downto 158);                    --
                                                                            --
  ------------------------------------------------------------------------------
  -- Instantiate the ILA Chipscope Component
  ------------------------------------------------------------------------------
  ila_cmp : chipscope_ila_1port_1trg256b
  port map (
    CONTROL => CONTROL1,
    CLK     => clk_int,
    TRIG0   => TRIG0);

  -- UDP RX signals

  TRIG0(0)              <= stb10;
  TRIG0(8 downto 1)     <= data2send(7 downto 0);
  TRIG0(9)              <= fifo_rden;
  TRIG0(10)             <= data_valid;
  TRIG0(26 downto 11)   <= std_logic_vector(data_array(to_integer(cnt10_wr))(15 downto 0));
  TRIG0(42 downto 27)   <= std_logic_vector(data_array(to_integer(cnt10_wr))(31 downto 16));
  TRIG0(74 downto 43)   <= std_logic_vector(cnt10_wr);
  TRIG0(90 downto 75)   <= "00000000"&udp_tx_int.data.data_out;

  TRIG0(91)             <= udp_if_complete;

  TRIG0(160)            <= button_rise_int;
  TRIG0(161)            <= fifo_valid;


--  TRIG0(0)              <= udp_rx_start_int;
--  TRIG0(8 downto 1)     <= udp_rx_int.data.data_in(7 downto 0);
--  TRIG0(9)              <= udp_rx_int.data.data_in_valid;
--  TRIG0(10)             <= udp_rx_int.data.data_in_last;
--  TRIG0(26 downto 11)   <= udp_rx_int.hdr.data_length(15 downto 0);
--  TRIG0(42 downto 27)   <= udp_rx_int.hdr.dst_port(15 downto 0);
--  TRIG0(74 downto 43)   <= udp_rx_int.hdr.src_ip_addr(31 downto 0);
--  TRIG0(90 downto 75)   <= udp_rx_int.hdr.src_port(15 downto 0);
--  TRIG0(91)             <= udp_rx_int.hdr.is_valid;
--  -- IP RX signals
--  TRIG0(107 downto 92)  <= ip_rx_hdr_int.data_length(15 downto 0);
--  TRIG0(111 downto 108) <= ip_rx_hdr_int.last_error_code(3 downto 0);
--  TRIG0(119 downto 112) <= ip_rx_hdr_int.num_frame_errors(7 downto 0);
--  TRIG0(127 downto 120) <= ip_rx_hdr_int.protocol(7 downto 0);
--  TRIG0(159 downto 128) <= ip_rx_hdr_int.src_ip_addr(31 downto 0);
--  TRIG0(160)            <= ip_rx_hdr_int.is_broadcast;
--  TRIG0(161)            <= ip_rx_hdr_int.is_valid;
--  -- LED Debug (from mac_v2_2)
--  TRIG0(164 downto 162) <= LEDs_int(2 downto 0);
  -- TX Result
  TRIG0(166 downto 165) <= udp_tx_result_int(1 downto 0);
  -- ack MAC and IP addr
  TRIG0(214 downto 167) <= our_mac(47 downto 0);
  TRIG0(246 downto 215) <= our_ip (31 downto 0);
  ---- ack FSM
  TRIG0(247) <= set_last;--udp_tx_last_int;
  TRIG0(248) <= udp_tx_start_int; 
  TRIG0(249) <= udp_tx_int.data.data_out_valid;--udp_tx_valid_int;
  TRIG0(250) <= udp_tx_data_out_ready_int;
  TRIG0(252 downto 251) <= state_status;

  ------------------------------------------------------------------------------
  end generate chipscope_4syn;                                                --
  ------------------------------------------------------------------------------









  end rtl;

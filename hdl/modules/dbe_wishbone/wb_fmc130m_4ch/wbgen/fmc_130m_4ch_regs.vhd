---------------------------------------------------------------------------------------
-- Title          : Wishbone slave core for Control and status registers for FMC 130M 4CH
---------------------------------------------------------------------------------------
-- File           : fmc_130m_4ch_regs.vhd
-- Author         : auto-generated by wbgen2 from fmc_130m_4ch_regs.wb
-- Created        : Mon Aug 26 18:37:02 2013
-- Standard       : VHDL'87
---------------------------------------------------------------------------------------
-- THIS FILE WAS GENERATED BY wbgen2 FROM SOURCE FILE fmc_130m_4ch_regs.wb
-- DO NOT HAND-EDIT UNLESS IT'S ABSOLUTELY NECESSARY!
---------------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.wb_fmc_130m_4ch_csr_wbgen2_pkg.all;


entity wb_fmc_130m_4ch_csr is
  port (
    rst_n_i                                  : in     std_logic;
    clk_sys_i                                : in     std_logic;
    wb_adr_i                                 : in     std_logic_vector(3 downto 0);
    wb_dat_i                                 : in     std_logic_vector(31 downto 0);
    wb_dat_o                                 : out    std_logic_vector(31 downto 0);
    wb_cyc_i                                 : in     std_logic;
    wb_sel_i                                 : in     std_logic_vector(3 downto 0);
    wb_stb_i                                 : in     std_logic;
    wb_we_i                                  : in     std_logic;
    wb_ack_o                                 : out    std_logic;
    wb_stall_o                               : out    std_logic;
    fs_clk_i                                 : in     std_logic;
    regs_i                                   : in     t_wb_fmc_130m_4ch_csr_in_registers;
    regs_o                                   : out    t_wb_fmc_130m_4ch_csr_out_registers
  );
end wb_fmc_130m_4ch_csr;

architecture syn of wb_fmc_130m_4ch_csr is

signal wb_fmc_130m_4ch_csr_trigger_dir_int      : std_logic      ;
signal wb_fmc_130m_4ch_csr_trigger_term_int     : std_logic      ;
signal wb_fmc_130m_4ch_csr_trigger_trig_val_int : std_logic      ;
signal wb_fmc_130m_4ch_csr_adc_rand_int         : std_logic      ;
signal wb_fmc_130m_4ch_csr_adc_dith_int         : std_logic      ;
signal wb_fmc_130m_4ch_csr_adc_shdn_int         : std_logic      ;
signal wb_fmc_130m_4ch_csr_adc_pga_int          : std_logic      ;
signal wb_fmc_130m_4ch_csr_clk_distrib_si571_oe_int : std_logic      ;
signal wb_fmc_130m_4ch_csr_clk_distrib_pll_function_int : std_logic      ;
signal wb_fmc_130m_4ch_csr_clk_distrib_clk_sel_int : std_logic      ;
signal wb_fmc_130m_4ch_csr_monitor_led1_int     : std_logic      ;
signal wb_fmc_130m_4ch_csr_monitor_led2_int     : std_logic      ;
signal wb_fmc_130m_4ch_csr_monitor_led3_int     : std_logic      ;
signal wb_fmc_130m_4ch_csr_fpga_ctrl_fmc_idelay_rst_int : std_logic      ;
signal wb_fmc_130m_4ch_csr_fpga_ctrl_fmc_fifo_rst_int : std_logic      ;
signal wb_fmc_130m_4ch_csr_fpga_ctrl_test_data_en_int : std_logic      ;
signal wb_fmc_130m_4ch_csr_idelay0_cal_update_dly0 : std_logic      ;
signal wb_fmc_130m_4ch_csr_idelay0_cal_update_int : std_logic      ;
signal wb_fmc_130m_4ch_csr_idelay0_cal_line_int : std_logic_vector(16 downto 0);
signal wb_fmc_130m_4ch_csr_idelay1_cal_update_dly0 : std_logic      ;
signal wb_fmc_130m_4ch_csr_idelay1_cal_update_int : std_logic      ;
signal wb_fmc_130m_4ch_csr_idelay1_cal_line_int : std_logic_vector(16 downto 0);
signal wb_fmc_130m_4ch_csr_idelay2_cal_update_dly0 : std_logic      ;
signal wb_fmc_130m_4ch_csr_idelay2_cal_update_int : std_logic      ;
signal wb_fmc_130m_4ch_csr_idelay2_cal_line_int : std_logic_vector(16 downto 0);
signal wb_fmc_130m_4ch_csr_idelay3_cal_update_dly0 : std_logic      ;
signal wb_fmc_130m_4ch_csr_idelay3_cal_update_int : std_logic      ;
signal wb_fmc_130m_4ch_csr_idelay3_cal_line_int : std_logic_vector(16 downto 0);
signal wb_fmc_130m_4ch_csr_data0_val_int        : std_logic_vector(31 downto 0);
signal wb_fmc_130m_4ch_csr_data0_val_lwb        : std_logic      ;
signal wb_fmc_130m_4ch_csr_data0_val_lwb_delay  : std_logic      ;
signal wb_fmc_130m_4ch_csr_data0_val_lwb_in_progress : std_logic      ;
signal wb_fmc_130m_4ch_csr_data0_val_lwb_s0     : std_logic      ;
signal wb_fmc_130m_4ch_csr_data0_val_lwb_s1     : std_logic      ;
signal wb_fmc_130m_4ch_csr_data0_val_lwb_s2     : std_logic      ;
signal wb_fmc_130m_4ch_csr_data1_val_int        : std_logic_vector(31 downto 0);
signal wb_fmc_130m_4ch_csr_data1_val_lwb        : std_logic      ;
signal wb_fmc_130m_4ch_csr_data1_val_lwb_delay  : std_logic      ;
signal wb_fmc_130m_4ch_csr_data1_val_lwb_in_progress : std_logic      ;
signal wb_fmc_130m_4ch_csr_data1_val_lwb_s0     : std_logic      ;
signal wb_fmc_130m_4ch_csr_data1_val_lwb_s1     : std_logic      ;
signal wb_fmc_130m_4ch_csr_data1_val_lwb_s2     : std_logic      ;
signal wb_fmc_130m_4ch_csr_data2_val_int        : std_logic_vector(31 downto 0);
signal wb_fmc_130m_4ch_csr_data2_val_lwb        : std_logic      ;
signal wb_fmc_130m_4ch_csr_data2_val_lwb_delay  : std_logic      ;
signal wb_fmc_130m_4ch_csr_data2_val_lwb_in_progress : std_logic      ;
signal wb_fmc_130m_4ch_csr_data2_val_lwb_s0     : std_logic      ;
signal wb_fmc_130m_4ch_csr_data2_val_lwb_s1     : std_logic      ;
signal wb_fmc_130m_4ch_csr_data2_val_lwb_s2     : std_logic      ;
signal wb_fmc_130m_4ch_csr_data3_val_int        : std_logic_vector(31 downto 0);
signal wb_fmc_130m_4ch_csr_data3_val_lwb        : std_logic      ;
signal wb_fmc_130m_4ch_csr_data3_val_lwb_delay  : std_logic      ;
signal wb_fmc_130m_4ch_csr_data3_val_lwb_in_progress : std_logic      ;
signal wb_fmc_130m_4ch_csr_data3_val_lwb_s0     : std_logic      ;
signal wb_fmc_130m_4ch_csr_data3_val_lwb_s1     : std_logic      ;
signal wb_fmc_130m_4ch_csr_data3_val_lwb_s2     : std_logic      ;
signal wb_fmc_130m_4ch_csr_dcm_adc_en_int       : std_logic      ;
signal wb_fmc_130m_4ch_csr_dcm_adc_phase_int    : std_logic      ;
signal wb_fmc_130m_4ch_csr_dcm_adc_reset_int    : std_logic      ;
signal ack_sreg                                 : std_logic_vector(9 downto 0);
signal rddata_reg                               : std_logic_vector(31 downto 0);
signal wrdata_reg                               : std_logic_vector(31 downto 0);
signal bwsel_reg                                : std_logic_vector(3 downto 0);
signal rwaddr_reg                               : std_logic_vector(3 downto 0);
signal ack_in_progress                          : std_logic      ;
signal wr_int                                   : std_logic      ;
signal rd_int                                   : std_logic      ;
signal allones                                  : std_logic_vector(31 downto 0);
signal allzeros                                 : std_logic_vector(31 downto 0);

begin
-- Some internal signals assignments. For (foreseen) compatibility with other bus standards.
  wrdata_reg <= wb_dat_i;
  bwsel_reg <= wb_sel_i;
  rd_int <= wb_cyc_i and (wb_stb_i and (not wb_we_i));
  wr_int <= wb_cyc_i and (wb_stb_i and wb_we_i);
  allones <= (others => '1');
  allzeros <= (others => '0');
-- 
-- Main register bank access process.
  process (clk_sys_i, rst_n_i)
  begin
    if (rst_n_i = '0') then 
      ack_sreg <= "0000000000";
      ack_in_progress <= '0';
      rddata_reg <= "00000000000000000000000000000000";
      wb_fmc_130m_4ch_csr_trigger_dir_int <= '0';
      wb_fmc_130m_4ch_csr_trigger_term_int <= '0';
      wb_fmc_130m_4ch_csr_trigger_trig_val_int <= '0';
      wb_fmc_130m_4ch_csr_adc_rand_int <= '0';
      wb_fmc_130m_4ch_csr_adc_dith_int <= '0';
      wb_fmc_130m_4ch_csr_adc_shdn_int <= '0';
      wb_fmc_130m_4ch_csr_adc_pga_int <= '0';
      wb_fmc_130m_4ch_csr_clk_distrib_si571_oe_int <= '0';
      wb_fmc_130m_4ch_csr_clk_distrib_pll_function_int <= '0';
      wb_fmc_130m_4ch_csr_clk_distrib_clk_sel_int <= '0';
      wb_fmc_130m_4ch_csr_monitor_led1_int <= '0';
      wb_fmc_130m_4ch_csr_monitor_led2_int <= '0';
      wb_fmc_130m_4ch_csr_monitor_led3_int <= '0';
      wb_fmc_130m_4ch_csr_fpga_ctrl_fmc_idelay_rst_int <= '0';
      wb_fmc_130m_4ch_csr_fpga_ctrl_fmc_fifo_rst_int <= '0';
      wb_fmc_130m_4ch_csr_fpga_ctrl_test_data_en_int <= '0';
      wb_fmc_130m_4ch_csr_idelay0_cal_update_int <= '0';
      wb_fmc_130m_4ch_csr_idelay0_cal_line_int <= "00000000000000000";
      regs_o.idelay0_cal_val_load_o <= '0';
      wb_fmc_130m_4ch_csr_idelay1_cal_update_int <= '0';
      wb_fmc_130m_4ch_csr_idelay1_cal_line_int <= "00000000000000000";
      regs_o.idelay1_cal_val_load_o <= '0';
      wb_fmc_130m_4ch_csr_idelay2_cal_update_int <= '0';
      wb_fmc_130m_4ch_csr_idelay2_cal_line_int <= "00000000000000000";
      regs_o.idelay2_cal_val_load_o <= '0';
      wb_fmc_130m_4ch_csr_idelay3_cal_update_int <= '0';
      wb_fmc_130m_4ch_csr_idelay3_cal_line_int <= "00000000000000000";
      regs_o.idelay3_cal_val_load_o <= '0';
      wb_fmc_130m_4ch_csr_data0_val_lwb <= '0';
      wb_fmc_130m_4ch_csr_data0_val_lwb_delay <= '0';
      wb_fmc_130m_4ch_csr_data0_val_lwb_in_progress <= '0';
      wb_fmc_130m_4ch_csr_data1_val_lwb <= '0';
      wb_fmc_130m_4ch_csr_data1_val_lwb_delay <= '0';
      wb_fmc_130m_4ch_csr_data1_val_lwb_in_progress <= '0';
      wb_fmc_130m_4ch_csr_data2_val_lwb <= '0';
      wb_fmc_130m_4ch_csr_data2_val_lwb_delay <= '0';
      wb_fmc_130m_4ch_csr_data2_val_lwb_in_progress <= '0';
      wb_fmc_130m_4ch_csr_data3_val_lwb <= '0';
      wb_fmc_130m_4ch_csr_data3_val_lwb_delay <= '0';
      wb_fmc_130m_4ch_csr_data3_val_lwb_in_progress <= '0';
      wb_fmc_130m_4ch_csr_dcm_adc_en_int <= '0';
      wb_fmc_130m_4ch_csr_dcm_adc_phase_int <= '0';
      wb_fmc_130m_4ch_csr_dcm_adc_reset_int <= '0';
    elsif rising_edge(clk_sys_i) then
-- advance the ACK generator shift register
      ack_sreg(8 downto 0) <= ack_sreg(9 downto 1);
      ack_sreg(9) <= '0';
      if (ack_in_progress = '1') then
        if (ack_sreg(0) = '1') then
          wb_fmc_130m_4ch_csr_idelay0_cal_update_int <= '0';
          regs_o.idelay0_cal_val_load_o <= '0';
          wb_fmc_130m_4ch_csr_idelay1_cal_update_int <= '0';
          regs_o.idelay1_cal_val_load_o <= '0';
          wb_fmc_130m_4ch_csr_idelay2_cal_update_int <= '0';
          regs_o.idelay2_cal_val_load_o <= '0';
          wb_fmc_130m_4ch_csr_idelay3_cal_update_int <= '0';
          regs_o.idelay3_cal_val_load_o <= '0';
          ack_in_progress <= '0';
        else
          regs_o.idelay0_cal_val_load_o <= '0';
          regs_o.idelay1_cal_val_load_o <= '0';
          regs_o.idelay2_cal_val_load_o <= '0';
          regs_o.idelay3_cal_val_load_o <= '0';
          wb_fmc_130m_4ch_csr_data0_val_lwb <= wb_fmc_130m_4ch_csr_data0_val_lwb_delay;
          wb_fmc_130m_4ch_csr_data0_val_lwb_delay <= '0';
          if ((ack_sreg(1) = '1') and (wb_fmc_130m_4ch_csr_data0_val_lwb_in_progress = '1')) then
            rddata_reg(31 downto 0) <= wb_fmc_130m_4ch_csr_data0_val_int;
            wb_fmc_130m_4ch_csr_data0_val_lwb_in_progress <= '0';
          end if;
          wb_fmc_130m_4ch_csr_data1_val_lwb <= wb_fmc_130m_4ch_csr_data1_val_lwb_delay;
          wb_fmc_130m_4ch_csr_data1_val_lwb_delay <= '0';
          if ((ack_sreg(1) = '1') and (wb_fmc_130m_4ch_csr_data1_val_lwb_in_progress = '1')) then
            rddata_reg(31 downto 0) <= wb_fmc_130m_4ch_csr_data1_val_int;
            wb_fmc_130m_4ch_csr_data1_val_lwb_in_progress <= '0';
          end if;
          wb_fmc_130m_4ch_csr_data2_val_lwb <= wb_fmc_130m_4ch_csr_data2_val_lwb_delay;
          wb_fmc_130m_4ch_csr_data2_val_lwb_delay <= '0';
          if ((ack_sreg(1) = '1') and (wb_fmc_130m_4ch_csr_data2_val_lwb_in_progress = '1')) then
            rddata_reg(31 downto 0) <= wb_fmc_130m_4ch_csr_data2_val_int;
            wb_fmc_130m_4ch_csr_data2_val_lwb_in_progress <= '0';
          end if;
          wb_fmc_130m_4ch_csr_data3_val_lwb <= wb_fmc_130m_4ch_csr_data3_val_lwb_delay;
          wb_fmc_130m_4ch_csr_data3_val_lwb_delay <= '0';
          if ((ack_sreg(1) = '1') and (wb_fmc_130m_4ch_csr_data3_val_lwb_in_progress = '1')) then
            rddata_reg(31 downto 0) <= wb_fmc_130m_4ch_csr_data3_val_int;
            wb_fmc_130m_4ch_csr_data3_val_lwb_in_progress <= '0';
          end if;
        end if;
      else
        if ((wb_cyc_i = '1') and (wb_stb_i = '1')) then
          case rwaddr_reg(3 downto 0) is
          when "0000" => 
            if (wb_we_i = '1') then
            end if;
            rddata_reg(0) <= regs_i.fmc_status_prsnt_i;
            rddata_reg(1) <= regs_i.fmc_status_pg_m2c_i;
            rddata_reg(2) <= regs_i.fmc_status_clk_dir_i;
            rddata_reg(31 downto 3) <= regs_i.fmc_status_firmware_id_i;
            ack_sreg(0) <= '1';
            ack_in_progress <= '1';
          when "0001" => 
            if (wb_we_i = '1') then
              wb_fmc_130m_4ch_csr_trigger_dir_int <= wrdata_reg(0);
              wb_fmc_130m_4ch_csr_trigger_term_int <= wrdata_reg(1);
              wb_fmc_130m_4ch_csr_trigger_trig_val_int <= wrdata_reg(2);
            end if;
            rddata_reg(0) <= wb_fmc_130m_4ch_csr_trigger_dir_int;
            rddata_reg(1) <= wb_fmc_130m_4ch_csr_trigger_term_int;
            rddata_reg(2) <= wb_fmc_130m_4ch_csr_trigger_trig_val_int;
            rddata_reg(31 downto 3) <= regs_i.trigger_reserved_i;
            ack_sreg(0) <= '1';
            ack_in_progress <= '1';
          when "0010" => 
            if (wb_we_i = '1') then
              wb_fmc_130m_4ch_csr_adc_rand_int <= wrdata_reg(0);
              wb_fmc_130m_4ch_csr_adc_dith_int <= wrdata_reg(1);
              wb_fmc_130m_4ch_csr_adc_shdn_int <= wrdata_reg(2);
              wb_fmc_130m_4ch_csr_adc_pga_int <= wrdata_reg(3);
            end if;
            rddata_reg(0) <= wb_fmc_130m_4ch_csr_adc_rand_int;
            rddata_reg(1) <= wb_fmc_130m_4ch_csr_adc_dith_int;
            rddata_reg(2) <= wb_fmc_130m_4ch_csr_adc_shdn_int;
            rddata_reg(3) <= wb_fmc_130m_4ch_csr_adc_pga_int;
            rddata_reg(31 downto 4) <= regs_i.adc_reserved_i;
            ack_sreg(0) <= '1';
            ack_in_progress <= '1';
          when "0011" => 
            if (wb_we_i = '1') then
              wb_fmc_130m_4ch_csr_clk_distrib_si571_oe_int <= wrdata_reg(0);
              wb_fmc_130m_4ch_csr_clk_distrib_pll_function_int <= wrdata_reg(1);
              wb_fmc_130m_4ch_csr_clk_distrib_clk_sel_int <= wrdata_reg(3);
            end if;
            rddata_reg(0) <= wb_fmc_130m_4ch_csr_clk_distrib_si571_oe_int;
            rddata_reg(1) <= wb_fmc_130m_4ch_csr_clk_distrib_pll_function_int;
            rddata_reg(2) <= regs_i.clk_distrib_pll_status_i;
            rddata_reg(3) <= wb_fmc_130m_4ch_csr_clk_distrib_clk_sel_int;
            rddata_reg(31 downto 4) <= regs_i.clk_distrib_reserved_i;
            ack_sreg(0) <= '1';
            ack_in_progress <= '1';
          when "0100" => 
            if (wb_we_i = '1') then
              wb_fmc_130m_4ch_csr_monitor_led1_int <= wrdata_reg(1);
              wb_fmc_130m_4ch_csr_monitor_led2_int <= wrdata_reg(2);
              wb_fmc_130m_4ch_csr_monitor_led3_int <= wrdata_reg(3);
            end if;
            rddata_reg(0) <= regs_i.monitor_temp_alarm_i;
            rddata_reg(1) <= wb_fmc_130m_4ch_csr_monitor_led1_int;
            rddata_reg(2) <= wb_fmc_130m_4ch_csr_monitor_led2_int;
            rddata_reg(3) <= wb_fmc_130m_4ch_csr_monitor_led3_int;
            rddata_reg(31 downto 4) <= regs_i.monitor_reserved_i;
            ack_sreg(0) <= '1';
            ack_in_progress <= '1';
          when "0101" => 
            if (wb_we_i = '1') then
              wb_fmc_130m_4ch_csr_fpga_ctrl_fmc_idelay_rst_int <= wrdata_reg(0);
              wb_fmc_130m_4ch_csr_fpga_ctrl_fmc_fifo_rst_int <= wrdata_reg(1);
              wb_fmc_130m_4ch_csr_fpga_ctrl_test_data_en_int <= wrdata_reg(8);
            end if;
            rddata_reg(0) <= wb_fmc_130m_4ch_csr_fpga_ctrl_fmc_idelay_rst_int;
            rddata_reg(1) <= wb_fmc_130m_4ch_csr_fpga_ctrl_fmc_fifo_rst_int;
            rddata_reg(2) <= regs_i.fpga_ctrl_fmc_idelay0_rdy_i;
            rddata_reg(3) <= regs_i.fpga_ctrl_fmc_idelay1_rdy_i;
            rddata_reg(4) <= regs_i.fpga_ctrl_fmc_idelay2_rdy_i;
            rddata_reg(5) <= regs_i.fpga_ctrl_fmc_idelay3_rdy_i;
            rddata_reg(7 downto 6) <= regs_i.fpga_ctrl_reserved1_i;
            rddata_reg(8) <= wb_fmc_130m_4ch_csr_fpga_ctrl_test_data_en_int;
            rddata_reg(31 downto 9) <= regs_i.fpga_ctrl_reserved2_i;
            ack_sreg(0) <= '1';
            ack_in_progress <= '1';
          when "0110" => 
            if (wb_we_i = '1') then
              wb_fmc_130m_4ch_csr_idelay0_cal_update_int <= wrdata_reg(0);
              wb_fmc_130m_4ch_csr_idelay0_cal_line_int <= wrdata_reg(17 downto 1);
              regs_o.idelay0_cal_val_load_o <= '1';
            end if;
            rddata_reg(0) <= '0';
            rddata_reg(17 downto 1) <= wb_fmc_130m_4ch_csr_idelay0_cal_line_int;
            rddata_reg(22 downto 18) <= regs_i.idelay0_cal_val_i;
            rddata_reg(31 downto 23) <= regs_i.idelay0_cal_reserved_i;
            ack_sreg(2) <= '1';
            ack_in_progress <= '1';
          when "0111" => 
            if (wb_we_i = '1') then
              wb_fmc_130m_4ch_csr_idelay1_cal_update_int <= wrdata_reg(0);
              wb_fmc_130m_4ch_csr_idelay1_cal_line_int <= wrdata_reg(17 downto 1);
              regs_o.idelay1_cal_val_load_o <= '1';
            end if;
            rddata_reg(0) <= '0';
            rddata_reg(17 downto 1) <= wb_fmc_130m_4ch_csr_idelay1_cal_line_int;
            rddata_reg(22 downto 18) <= regs_i.idelay1_cal_val_i;
            rddata_reg(31 downto 23) <= regs_i.idelay1_cal_reserved_i;
            ack_sreg(2) <= '1';
            ack_in_progress <= '1';
          when "1000" => 
            if (wb_we_i = '1') then
              wb_fmc_130m_4ch_csr_idelay2_cal_update_int <= wrdata_reg(0);
              wb_fmc_130m_4ch_csr_idelay2_cal_line_int <= wrdata_reg(17 downto 1);
              regs_o.idelay2_cal_val_load_o <= '1';
            end if;
            rddata_reg(0) <= '0';
            rddata_reg(17 downto 1) <= wb_fmc_130m_4ch_csr_idelay2_cal_line_int;
            rddata_reg(22 downto 18) <= regs_i.idelay2_cal_val_i;
            rddata_reg(31 downto 23) <= regs_i.idelay2_cal_reserved_i;
            ack_sreg(2) <= '1';
            ack_in_progress <= '1';
          when "1001" => 
            if (wb_we_i = '1') then
              wb_fmc_130m_4ch_csr_idelay3_cal_update_int <= wrdata_reg(0);
              wb_fmc_130m_4ch_csr_idelay3_cal_line_int <= wrdata_reg(17 downto 1);
              regs_o.idelay3_cal_val_load_o <= '1';
            end if;
            rddata_reg(0) <= '0';
            rddata_reg(17 downto 1) <= wb_fmc_130m_4ch_csr_idelay3_cal_line_int;
            rddata_reg(22 downto 18) <= regs_i.idelay3_cal_val_i;
            rddata_reg(31 downto 23) <= regs_i.idelay3_cal_reserved_i;
            ack_sreg(2) <= '1';
            ack_in_progress <= '1';
          when "1010" => 
            if (wb_we_i = '1') then
            end if;
            if (wb_we_i = '0') then
              wb_fmc_130m_4ch_csr_data0_val_lwb <= '1';
              wb_fmc_130m_4ch_csr_data0_val_lwb_delay <= '1';
              wb_fmc_130m_4ch_csr_data0_val_lwb_in_progress <= '1';
            end if;
            ack_sreg(5) <= '1';
            ack_in_progress <= '1';
          when "1011" => 
            if (wb_we_i = '1') then
            end if;
            if (wb_we_i = '0') then
              wb_fmc_130m_4ch_csr_data1_val_lwb <= '1';
              wb_fmc_130m_4ch_csr_data1_val_lwb_delay <= '1';
              wb_fmc_130m_4ch_csr_data1_val_lwb_in_progress <= '1';
            end if;
            ack_sreg(5) <= '1';
            ack_in_progress <= '1';
          when "1100" => 
            if (wb_we_i = '1') then
            end if;
            if (wb_we_i = '0') then
              wb_fmc_130m_4ch_csr_data2_val_lwb <= '1';
              wb_fmc_130m_4ch_csr_data2_val_lwb_delay <= '1';
              wb_fmc_130m_4ch_csr_data2_val_lwb_in_progress <= '1';
            end if;
            ack_sreg(5) <= '1';
            ack_in_progress <= '1';
          when "1101" => 
            if (wb_we_i = '1') then
            end if;
            if (wb_we_i = '0') then
              wb_fmc_130m_4ch_csr_data3_val_lwb <= '1';
              wb_fmc_130m_4ch_csr_data3_val_lwb_delay <= '1';
              wb_fmc_130m_4ch_csr_data3_val_lwb_in_progress <= '1';
            end if;
            ack_sreg(5) <= '1';
            ack_in_progress <= '1';
          when "1110" => 
            if (wb_we_i = '1') then
              wb_fmc_130m_4ch_csr_dcm_adc_en_int <= wrdata_reg(0);
              wb_fmc_130m_4ch_csr_dcm_adc_phase_int <= wrdata_reg(1);
              wb_fmc_130m_4ch_csr_dcm_adc_reset_int <= wrdata_reg(4);
            end if;
            rddata_reg(0) <= wb_fmc_130m_4ch_csr_dcm_adc_en_int;
            rddata_reg(1) <= wb_fmc_130m_4ch_csr_dcm_adc_phase_int;
            rddata_reg(2) <= regs_i.dcm_adc_done_i;
            rddata_reg(3) <= regs_i.dcm_adc_status0_i;
            rddata_reg(4) <= wb_fmc_130m_4ch_csr_dcm_adc_reset_int;
            rddata_reg(31 downto 5) <= regs_i.dcm_reserved_i;
            ack_sreg(0) <= '1';
            ack_in_progress <= '1';
          when others =>
-- prevent the slave from hanging the bus on invalid address
            ack_in_progress <= '1';
            ack_sreg(0) <= '1';
          end case;
        end if;
      end if;
    end if;
  end process;
  
  
-- Drive the data output bus
  wb_dat_o <= rddata_reg;
-- FMC Present
-- Power Good from mezzanine
-- Clock Direction
-- Firware ID
-- DIR
  regs_o.trigger_dir_o <= wb_fmc_130m_4ch_csr_trigger_dir_int;
-- Termination Control
  regs_o.trigger_term_o <= wb_fmc_130m_4ch_csr_trigger_term_int;
-- Trigger Value
  regs_o.trigger_trig_val_o <= wb_fmc_130m_4ch_csr_trigger_trig_val_int;
-- Reserved
-- RAND
  regs_o.adc_rand_o <= wb_fmc_130m_4ch_csr_adc_rand_int;
-- DITH
  regs_o.adc_dith_o <= wb_fmc_130m_4ch_csr_adc_dith_int;
-- SHDN
  regs_o.adc_shdn_o <= wb_fmc_130m_4ch_csr_adc_shdn_int;
-- PGA
  regs_o.adc_pga_o <= wb_fmc_130m_4ch_csr_adc_pga_int;
-- Reserved
-- SI571_OE
  regs_o.clk_distrib_si571_oe_o <= wb_fmc_130m_4ch_csr_clk_distrib_si571_oe_int;
-- PLL_FUNCTION
  regs_o.clk_distrib_pll_function_o <= wb_fmc_130m_4ch_csr_clk_distrib_pll_function_int;
-- PLL_STATUS
-- CLK_SEL
  regs_o.clk_distrib_clk_sel_o <= wb_fmc_130m_4ch_csr_clk_distrib_clk_sel_int;
-- Reserved
-- Temperate Alarm
-- Led 1
  regs_o.monitor_led1_o <= wb_fmc_130m_4ch_csr_monitor_led1_int;
-- Led 2
  regs_o.monitor_led2_o <= wb_fmc_130m_4ch_csr_monitor_led2_int;
-- Led 3
  regs_o.monitor_led3_o <= wb_fmc_130m_4ch_csr_monitor_led3_int;
-- Reserved
-- FMC_IDELAY_RST
  regs_o.fpga_ctrl_fmc_idelay_rst_o <= wb_fmc_130m_4ch_csr_fpga_ctrl_fmc_idelay_rst_int;
-- FMC_FIFO_RST
  regs_o.fpga_ctrl_fmc_fifo_rst_o <= wb_fmc_130m_4ch_csr_fpga_ctrl_fmc_fifo_rst_int;
-- FMC_IDELAY0_RDY
-- FMC_IDELAY1_RDY
-- FMC_IDELAY2_RDY
-- FMC_IDELAY3_RDY
-- Reserved
-- Enable test data
  regs_o.fpga_ctrl_test_data_en_o <= wb_fmc_130m_4ch_csr_fpga_ctrl_test_data_en_int;
-- Reserved
-- UPDATE
  process (clk_sys_i, rst_n_i)
  begin
    if (rst_n_i = '0') then 
      wb_fmc_130m_4ch_csr_idelay0_cal_update_dly0 <= '0';
      regs_o.idelay0_cal_update_o <= '0';
    elsif rising_edge(clk_sys_i) then
      wb_fmc_130m_4ch_csr_idelay0_cal_update_dly0 <= wb_fmc_130m_4ch_csr_idelay0_cal_update_int;
      regs_o.idelay0_cal_update_o <= wb_fmc_130m_4ch_csr_idelay0_cal_update_int and (not wb_fmc_130m_4ch_csr_idelay0_cal_update_dly0);
    end if;
  end process;
  
  
-- LINE
  regs_o.idelay0_cal_line_o <= wb_fmc_130m_4ch_csr_idelay0_cal_line_int;
-- VAL
  regs_o.idelay0_cal_val_o <= wrdata_reg(22 downto 18);
-- Reserved
-- UPDATE
  process (clk_sys_i, rst_n_i)
  begin
    if (rst_n_i = '0') then 
      wb_fmc_130m_4ch_csr_idelay1_cal_update_dly0 <= '0';
      regs_o.idelay1_cal_update_o <= '0';
    elsif rising_edge(clk_sys_i) then
      wb_fmc_130m_4ch_csr_idelay1_cal_update_dly0 <= wb_fmc_130m_4ch_csr_idelay1_cal_update_int;
      regs_o.idelay1_cal_update_o <= wb_fmc_130m_4ch_csr_idelay1_cal_update_int and (not wb_fmc_130m_4ch_csr_idelay1_cal_update_dly0);
    end if;
  end process;
  
  
-- LINE
  regs_o.idelay1_cal_line_o <= wb_fmc_130m_4ch_csr_idelay1_cal_line_int;
-- VAL
  regs_o.idelay1_cal_val_o <= wrdata_reg(22 downto 18);
-- Reserved
-- UPDATE
  process (clk_sys_i, rst_n_i)
  begin
    if (rst_n_i = '0') then 
      wb_fmc_130m_4ch_csr_idelay2_cal_update_dly0 <= '0';
      regs_o.idelay2_cal_update_o <= '0';
    elsif rising_edge(clk_sys_i) then
      wb_fmc_130m_4ch_csr_idelay2_cal_update_dly0 <= wb_fmc_130m_4ch_csr_idelay2_cal_update_int;
      regs_o.idelay2_cal_update_o <= wb_fmc_130m_4ch_csr_idelay2_cal_update_int and (not wb_fmc_130m_4ch_csr_idelay2_cal_update_dly0);
    end if;
  end process;
  
  
-- LINE
  regs_o.idelay2_cal_line_o <= wb_fmc_130m_4ch_csr_idelay2_cal_line_int;
-- VAL
  regs_o.idelay2_cal_val_o <= wrdata_reg(22 downto 18);
-- Reserved
-- UPDATE
  process (clk_sys_i, rst_n_i)
  begin
    if (rst_n_i = '0') then 
      wb_fmc_130m_4ch_csr_idelay3_cal_update_dly0 <= '0';
      regs_o.idelay3_cal_update_o <= '0';
    elsif rising_edge(clk_sys_i) then
      wb_fmc_130m_4ch_csr_idelay3_cal_update_dly0 <= wb_fmc_130m_4ch_csr_idelay3_cal_update_int;
      regs_o.idelay3_cal_update_o <= wb_fmc_130m_4ch_csr_idelay3_cal_update_int and (not wb_fmc_130m_4ch_csr_idelay3_cal_update_dly0);
    end if;
  end process;
  
  
-- LINE
  regs_o.idelay3_cal_line_o <= wb_fmc_130m_4ch_csr_idelay3_cal_line_int;
-- VAL
  regs_o.idelay3_cal_val_o <= wrdata_reg(22 downto 18);
-- Reserved
-- DATA0
-- asynchronous std_logic_vector register : DATA0 (type RO/WO, fs_clk_i <-> clk_sys_i)
  process (fs_clk_i, rst_n_i)
  begin
    if (rst_n_i = '0') then 
      wb_fmc_130m_4ch_csr_data0_val_lwb_s0 <= '0';
      wb_fmc_130m_4ch_csr_data0_val_lwb_s1 <= '0';
      wb_fmc_130m_4ch_csr_data0_val_lwb_s2 <= '0';
      wb_fmc_130m_4ch_csr_data0_val_int <= "00000000000000000000000000000000";
    elsif rising_edge(fs_clk_i) then
      wb_fmc_130m_4ch_csr_data0_val_lwb_s0 <= wb_fmc_130m_4ch_csr_data0_val_lwb;
      wb_fmc_130m_4ch_csr_data0_val_lwb_s1 <= wb_fmc_130m_4ch_csr_data0_val_lwb_s0;
      wb_fmc_130m_4ch_csr_data0_val_lwb_s2 <= wb_fmc_130m_4ch_csr_data0_val_lwb_s1;
      if ((wb_fmc_130m_4ch_csr_data0_val_lwb_s1 = '1') and (wb_fmc_130m_4ch_csr_data0_val_lwb_s2 = '0')) then
        wb_fmc_130m_4ch_csr_data0_val_int <= regs_i.data0_val_i;
      end if;
    end if;
  end process;
  
  
-- DATA1
-- asynchronous std_logic_vector register : DATA1 (type RO/WO, fs_clk_i <-> clk_sys_i)
  process (fs_clk_i, rst_n_i)
  begin
    if (rst_n_i = '0') then 
      wb_fmc_130m_4ch_csr_data1_val_lwb_s0 <= '0';
      wb_fmc_130m_4ch_csr_data1_val_lwb_s1 <= '0';
      wb_fmc_130m_4ch_csr_data1_val_lwb_s2 <= '0';
      wb_fmc_130m_4ch_csr_data1_val_int <= "00000000000000000000000000000000";
    elsif rising_edge(fs_clk_i) then
      wb_fmc_130m_4ch_csr_data1_val_lwb_s0 <= wb_fmc_130m_4ch_csr_data1_val_lwb;
      wb_fmc_130m_4ch_csr_data1_val_lwb_s1 <= wb_fmc_130m_4ch_csr_data1_val_lwb_s0;
      wb_fmc_130m_4ch_csr_data1_val_lwb_s2 <= wb_fmc_130m_4ch_csr_data1_val_lwb_s1;
      if ((wb_fmc_130m_4ch_csr_data1_val_lwb_s1 = '1') and (wb_fmc_130m_4ch_csr_data1_val_lwb_s2 = '0')) then
        wb_fmc_130m_4ch_csr_data1_val_int <= regs_i.data1_val_i;
      end if;
    end if;
  end process;
  
  
-- DATA2
-- asynchronous std_logic_vector register : DATA2 (type RO/WO, fs_clk_i <-> clk_sys_i)
  process (fs_clk_i, rst_n_i)
  begin
    if (rst_n_i = '0') then 
      wb_fmc_130m_4ch_csr_data2_val_lwb_s0 <= '0';
      wb_fmc_130m_4ch_csr_data2_val_lwb_s1 <= '0';
      wb_fmc_130m_4ch_csr_data2_val_lwb_s2 <= '0';
      wb_fmc_130m_4ch_csr_data2_val_int <= "00000000000000000000000000000000";
    elsif rising_edge(fs_clk_i) then
      wb_fmc_130m_4ch_csr_data2_val_lwb_s0 <= wb_fmc_130m_4ch_csr_data2_val_lwb;
      wb_fmc_130m_4ch_csr_data2_val_lwb_s1 <= wb_fmc_130m_4ch_csr_data2_val_lwb_s0;
      wb_fmc_130m_4ch_csr_data2_val_lwb_s2 <= wb_fmc_130m_4ch_csr_data2_val_lwb_s1;
      if ((wb_fmc_130m_4ch_csr_data2_val_lwb_s1 = '1') and (wb_fmc_130m_4ch_csr_data2_val_lwb_s2 = '0')) then
        wb_fmc_130m_4ch_csr_data2_val_int <= regs_i.data2_val_i;
      end if;
    end if;
  end process;
  
  
-- DATA3
-- asynchronous std_logic_vector register : DATA3 (type RO/WO, fs_clk_i <-> clk_sys_i)
  process (fs_clk_i, rst_n_i)
  begin
    if (rst_n_i = '0') then 
      wb_fmc_130m_4ch_csr_data3_val_lwb_s0 <= '0';
      wb_fmc_130m_4ch_csr_data3_val_lwb_s1 <= '0';
      wb_fmc_130m_4ch_csr_data3_val_lwb_s2 <= '0';
      wb_fmc_130m_4ch_csr_data3_val_int <= "00000000000000000000000000000000";
    elsif rising_edge(fs_clk_i) then
      wb_fmc_130m_4ch_csr_data3_val_lwb_s0 <= wb_fmc_130m_4ch_csr_data3_val_lwb;
      wb_fmc_130m_4ch_csr_data3_val_lwb_s1 <= wb_fmc_130m_4ch_csr_data3_val_lwb_s0;
      wb_fmc_130m_4ch_csr_data3_val_lwb_s2 <= wb_fmc_130m_4ch_csr_data3_val_lwb_s1;
      if ((wb_fmc_130m_4ch_csr_data3_val_lwb_s1 = '1') and (wb_fmc_130m_4ch_csr_data3_val_lwb_s2 = '0')) then
        wb_fmc_130m_4ch_csr_data3_val_int <= regs_i.data3_val_i;
      end if;
    end if;
  end process;
  
  
-- ADC_DCM
  regs_o.dcm_adc_en_o <= wb_fmc_130m_4ch_csr_dcm_adc_en_int;
-- ADC_PHASE_INC
  regs_o.dcm_adc_phase_o <= wb_fmc_130m_4ch_csr_dcm_adc_phase_int;
-- ADC_DCM_DONE
-- ADC_DCM_STATUS0
-- ADC_RESET
  regs_o.dcm_adc_reset_o <= wb_fmc_130m_4ch_csr_dcm_adc_reset_int;
-- Reserved
  rwaddr_reg <= wb_adr_i;
  wb_stall_o <= (not ack_sreg(0)) and (wb_stb_i and wb_cyc_i);
-- ACK signal generation. Just pass the LSB of ACK counter.
  wb_ack_o <= ack_sreg(0);
end syn;

---------------------------------------------------------------------------------------
-- Title          : Wishbone slave core for FMC ADC 250MS/s core registers
---------------------------------------------------------------------------------------
-- File           : wb_fmc516_regs_pkg.vhd
-- Author         : auto-generated by wbgen2 from wb_fmc516_regs.wb
-- Created        : Sat Dec  8 10:59:15 2012
-- Standard       : VHDL'87
---------------------------------------------------------------------------------------
-- THIS FILE WAS GENERATED BY wbgen2 FROM SOURCE FILE wb_fmc516_regs.wb
-- DO NOT HAND-EDIT UNLESS IT'S ABSOLUTELY NECESSARY!
---------------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package fmc516_wbgen2_pkg is
  
  
  -- Input registers (user design -> WB slave)
  
  type t_fmc516_in_registers is record
    fmc_sta_lmk_locked_i                     : std_logic;
    fmc_sta_mmcm_locked_i                    : std_logic;
    fmc_sta_pwr_good_i                       : std_logic;
    fmc_sta_prst_i                           : std_logic;
    fmc_sta_reserved_i                       : std_logic_vector(27 downto 0);
    adc_sta_clk_chains_i                     : std_logic_vector(3 downto 0);
    adc_sta_reserved_clk_chains_i            : std_logic_vector(3 downto 0);
    adc_sta_data_chains_i                    : std_logic_vector(3 downto 0);
    adc_sta_reserved_data_chains_i           : std_logic_vector(3 downto 0);
    adc_sta_adc_pkt_size_i                   : std_logic_vector(15 downto 0);
    adc_ctl_reserved_i                       : std_logic_vector(28 downto 0);
    ch0_sta_val_i                            : std_logic_vector(15 downto 0);
    ch0_sta_reserved_i                       : std_logic_vector(15 downto 0);
    ch0_ctl_clk_chain_dly_i                  : std_logic_vector(4 downto 0);
    ch0_ctl_reserved_clk_chain_dly_i         : std_logic_vector(2 downto 0);
    ch0_ctl_data_chain_dly_i                 : std_logic_vector(4 downto 0);
    ch0_ctl_reserved_data_chain_dly_i        : std_logic_vector(2 downto 0);
    ch1_sta_val_i                            : std_logic_vector(15 downto 0);
    ch1_sta_reserved_i                       : std_logic_vector(15 downto 0);
    ch1_ctl_clk_chain_dly_i                  : std_logic_vector(4 downto 0);
    ch1_ctl_reserved_clk_chain_dly_i         : std_logic_vector(2 downto 0);
    ch1_ctl_data_chain_dly_i                 : std_logic_vector(4 downto 0);
    ch1_ctl_reserved_data_chain_dly_i        : std_logic_vector(2 downto 0);
    ch2_sta_val_i                            : std_logic_vector(15 downto 0);
    ch2_sta_reserved_i                       : std_logic_vector(15 downto 0);
    ch2_ctl_clk_chain_dly_i                  : std_logic_vector(4 downto 0);
    ch2_ctl_reserved_clk_chain_dly_i         : std_logic_vector(2 downto 0);
    ch2_ctl_data_chain_dly_i                 : std_logic_vector(4 downto 0);
    ch2_ctl_reserved_data_chain_dly_i        : std_logic_vector(2 downto 0);
    ch3_sta_val_i                            : std_logic_vector(15 downto 0);
    ch3_sta_reserved_i                       : std_logic_vector(15 downto 0);
    ch3_ctl_clk_chain_dly_i                  : std_logic_vector(4 downto 0);
    ch3_ctl_reserved_clk_chain_dly_i         : std_logic_vector(2 downto 0);
    ch3_ctl_data_chain_dly_i                 : std_logic_vector(4 downto 0);
    ch3_ctl_reserved_data_chain_dly_i        : std_logic_vector(2 downto 0);
    end record;
  
  constant c_fmc516_in_registers_init_value: t_fmc516_in_registers := (
    fmc_sta_lmk_locked_i => '0',
    fmc_sta_mmcm_locked_i => '0',
    fmc_sta_pwr_good_i => '0',
    fmc_sta_prst_i => '0',
    fmc_sta_reserved_i => (others => '0'),
    adc_sta_clk_chains_i => (others => '0'),
    adc_sta_reserved_clk_chains_i => (others => '0'),
    adc_sta_data_chains_i => (others => '0'),
    adc_sta_reserved_data_chains_i => (others => '0'),
    adc_sta_adc_pkt_size_i => (others => '0'),
    adc_ctl_reserved_i => (others => '0'),
    ch0_sta_val_i => (others => '0'),
    ch0_sta_reserved_i => (others => '0'),
    ch0_ctl_clk_chain_dly_i => (others => '0'),
    ch0_ctl_reserved_clk_chain_dly_i => (others => '0'),
    ch0_ctl_data_chain_dly_i => (others => '0'),
    ch0_ctl_reserved_data_chain_dly_i => (others => '0'),
    ch1_sta_val_i => (others => '0'),
    ch1_sta_reserved_i => (others => '0'),
    ch1_ctl_clk_chain_dly_i => (others => '0'),
    ch1_ctl_reserved_clk_chain_dly_i => (others => '0'),
    ch1_ctl_data_chain_dly_i => (others => '0'),
    ch1_ctl_reserved_data_chain_dly_i => (others => '0'),
    ch2_sta_val_i => (others => '0'),
    ch2_sta_reserved_i => (others => '0'),
    ch2_ctl_clk_chain_dly_i => (others => '0'),
    ch2_ctl_reserved_clk_chain_dly_i => (others => '0'),
    ch2_ctl_data_chain_dly_i => (others => '0'),
    ch2_ctl_reserved_data_chain_dly_i => (others => '0'),
    ch3_sta_val_i => (others => '0'),
    ch3_sta_reserved_i => (others => '0'),
    ch3_ctl_clk_chain_dly_i => (others => '0'),
    ch3_ctl_reserved_clk_chain_dly_i => (others => '0'),
    ch3_ctl_data_chain_dly_i => (others => '0'),
    ch3_ctl_reserved_data_chain_dly_i => (others => '0')
    );
    
    -- Output registers (WB slave -> user design)
    
    type t_fmc516_out_registers is record
      fmc_ctl_test_data_en_o                   : std_logic;
      fmc_ctl_led_0_o                          : std_logic;
      fmc_ctl_led_1_o                          : std_logic;
      fmc_ctl_clk_sel_o                        : std_logic;
      fmc_ctl_vcxo_out_en_o                    : std_logic;
      fmc_ctl_reserved_o                       : std_logic_vector(26 downto 0);
      trig_cfg_hw_trig_pol_o                   : std_logic;
      trig_cfg_hw_trig_en_o                    : std_logic;
      trig_cfg_reserved_o                      : std_logic_vector(29 downto 0);
      adc_ctl_update_dly_o                     : std_logic;
      adc_ctl_rst_adcs_o                       : std_logic;
      adc_ctl_rst_div_adcs_o                   : std_logic;
      ch0_ctl_clk_chain_dly_o                  : std_logic_vector(4 downto 0);
      ch0_ctl_clk_chain_dly_load_o             : std_logic;
      ch0_ctl_data_chain_dly_o                 : std_logic_vector(4 downto 0);
      ch0_ctl_data_chain_dly_load_o            : std_logic;
      ch0_ctl_inc_clk_chain_dly_o              : std_logic;
      ch0_ctl_dec_clk_chain_dly_o              : std_logic;
      ch0_ctl_reserved_clk_incdec_dly_o        : std_logic_vector(5 downto 0);
      ch0_ctl_inc_data_chain_dly_o             : std_logic;
      ch0_ctl_dec_data_chain_dly_o             : std_logic;
      ch0_ctl_reserved_data_incdec_dly_o       : std_logic_vector(5 downto 0);
      ch1_ctl_clk_chain_dly_o                  : std_logic_vector(4 downto 0);
      ch1_ctl_clk_chain_dly_load_o             : std_logic;
      ch1_ctl_data_chain_dly_o                 : std_logic_vector(4 downto 0);
      ch1_ctl_data_chain_dly_load_o            : std_logic;
      ch1_ctl_inc_clk_chain_dly_o              : std_logic;
      ch1_ctl_dec_clk_chain_dly_o              : std_logic;
      ch1_ctl_reserved_clk_incdec_dly_o        : std_logic_vector(5 downto 0);
      ch1_ctl_inc_data_chain_dly_o             : std_logic;
      ch1_ctl_dec_data_chain_dly_o             : std_logic;
      ch1_ctl_reserved_data_incdec_dly_o       : std_logic_vector(5 downto 0);
      ch2_ctl_clk_chain_dly_o                  : std_logic_vector(4 downto 0);
      ch2_ctl_clk_chain_dly_load_o             : std_logic;
      ch2_ctl_data_chain_dly_o                 : std_logic_vector(4 downto 0);
      ch2_ctl_data_chain_dly_load_o            : std_logic;
      ch2_ctl_inc_clk_chain_dly_o              : std_logic;
      ch2_ctl_dec_clk_chain_dly_o              : std_logic;
      ch2_ctl_reserved_clk_incdec_dly_o        : std_logic_vector(5 downto 0);
      ch2_ctl_inc_data_chain_dly_o             : std_logic;
      ch2_ctl_dec_data_chain_dly_o             : std_logic;
      ch2_ctl_reserved_data_incdec_dly_o       : std_logic_vector(5 downto 0);
      ch3_ctl_clk_chain_dly_o                  : std_logic_vector(4 downto 0);
      ch3_ctl_clk_chain_dly_load_o             : std_logic;
      ch3_ctl_data_chain_dly_o                 : std_logic_vector(4 downto 0);
      ch3_ctl_data_chain_dly_load_o            : std_logic;
      ch3_ctl_inc_clk_chain_dly_o              : std_logic;
      ch3_ctl_dec_clk_chain_dly_o              : std_logic;
      ch3_ctl_reserved_clk_incdec_dly_o        : std_logic_vector(5 downto 0);
      ch3_ctl_inc_data_chain_dly_o             : std_logic;
      ch3_ctl_dec_data_chain_dly_o             : std_logic;
      ch3_ctl_reserved_data_incdec_dly_o       : std_logic_vector(5 downto 0);
      end record;
    
    constant c_fmc516_out_registers_init_value: t_fmc516_out_registers := (
      fmc_ctl_test_data_en_o => '0',
      fmc_ctl_led_0_o => '0',
      fmc_ctl_led_1_o => '0',
      fmc_ctl_clk_sel_o => '0',
      fmc_ctl_vcxo_out_en_o => '0',
      fmc_ctl_reserved_o => (others => '0'),
      trig_cfg_hw_trig_pol_o => '0',
      trig_cfg_hw_trig_en_o => '0',
      trig_cfg_reserved_o => (others => '0'),
      adc_ctl_update_dly_o => '0',
      adc_ctl_rst_adcs_o => '0',
      adc_ctl_rst_div_adcs_o => '0',
      ch0_ctl_clk_chain_dly_o => (others => '0'),
      ch0_ctl_clk_chain_dly_load_o => '0',
      ch0_ctl_data_chain_dly_o => (others => '0'),
      ch0_ctl_data_chain_dly_load_o => '0',
      ch0_ctl_inc_clk_chain_dly_o => '0',
      ch0_ctl_dec_clk_chain_dly_o => '0',
      ch0_ctl_reserved_clk_incdec_dly_o => (others => '0'),
      ch0_ctl_inc_data_chain_dly_o => '0',
      ch0_ctl_dec_data_chain_dly_o => '0',
      ch0_ctl_reserved_data_incdec_dly_o => (others => '0'),
      ch1_ctl_clk_chain_dly_o => (others => '0'),
      ch1_ctl_clk_chain_dly_load_o => '0',
      ch1_ctl_data_chain_dly_o => (others => '0'),
      ch1_ctl_data_chain_dly_load_o => '0',
      ch1_ctl_inc_clk_chain_dly_o => '0',
      ch1_ctl_dec_clk_chain_dly_o => '0',
      ch1_ctl_reserved_clk_incdec_dly_o => (others => '0'),
      ch1_ctl_inc_data_chain_dly_o => '0',
      ch1_ctl_dec_data_chain_dly_o => '0',
      ch1_ctl_reserved_data_incdec_dly_o => (others => '0'),
      ch2_ctl_clk_chain_dly_o => (others => '0'),
      ch2_ctl_clk_chain_dly_load_o => '0',
      ch2_ctl_data_chain_dly_o => (others => '0'),
      ch2_ctl_data_chain_dly_load_o => '0',
      ch2_ctl_inc_clk_chain_dly_o => '0',
      ch2_ctl_dec_clk_chain_dly_o => '0',
      ch2_ctl_reserved_clk_incdec_dly_o => (others => '0'),
      ch2_ctl_inc_data_chain_dly_o => '0',
      ch2_ctl_dec_data_chain_dly_o => '0',
      ch2_ctl_reserved_data_incdec_dly_o => (others => '0'),
      ch3_ctl_clk_chain_dly_o => (others => '0'),
      ch3_ctl_clk_chain_dly_load_o => '0',
      ch3_ctl_data_chain_dly_o => (others => '0'),
      ch3_ctl_data_chain_dly_load_o => '0',
      ch3_ctl_inc_clk_chain_dly_o => '0',
      ch3_ctl_dec_clk_chain_dly_o => '0',
      ch3_ctl_reserved_clk_incdec_dly_o => (others => '0'),
      ch3_ctl_inc_data_chain_dly_o => '0',
      ch3_ctl_dec_data_chain_dly_o => '0',
      ch3_ctl_reserved_data_incdec_dly_o => (others => '0')
      );
    function "or" (left, right: t_fmc516_in_registers) return t_fmc516_in_registers;
    function f_x_to_zero (x:std_logic) return std_logic;
    function f_x_to_zero (x:std_logic_vector) return std_logic_vector;
end package;

package body fmc516_wbgen2_pkg is
function f_x_to_zero (x:std_logic) return std_logic is
begin
if(x = 'X' or x = 'U') then
return '0';
else
return x;
end if; 
end function;
function f_x_to_zero (x:std_logic_vector) return std_logic_vector is
variable tmp: std_logic_vector(x'length-1 downto 0);
begin
for i in 0 to x'length-1 loop
if(x(i) = 'X' or x(i) = 'U') then
tmp(i):= '0';
else
tmp(i):=x(i);
end if; 
end loop; 
return tmp;
end function;
function "or" (left, right: t_fmc516_in_registers) return t_fmc516_in_registers is
variable tmp: t_fmc516_in_registers;
begin
tmp.fmc_sta_lmk_locked_i := f_x_to_zero(left.fmc_sta_lmk_locked_i) or f_x_to_zero(right.fmc_sta_lmk_locked_i);
tmp.fmc_sta_mmcm_locked_i := f_x_to_zero(left.fmc_sta_mmcm_locked_i) or f_x_to_zero(right.fmc_sta_mmcm_locked_i);
tmp.fmc_sta_pwr_good_i := f_x_to_zero(left.fmc_sta_pwr_good_i) or f_x_to_zero(right.fmc_sta_pwr_good_i);
tmp.fmc_sta_prst_i := f_x_to_zero(left.fmc_sta_prst_i) or f_x_to_zero(right.fmc_sta_prst_i);
tmp.fmc_sta_reserved_i := f_x_to_zero(left.fmc_sta_reserved_i) or f_x_to_zero(right.fmc_sta_reserved_i);
tmp.adc_sta_clk_chains_i := f_x_to_zero(left.adc_sta_clk_chains_i) or f_x_to_zero(right.adc_sta_clk_chains_i);
tmp.adc_sta_reserved_clk_chains_i := f_x_to_zero(left.adc_sta_reserved_clk_chains_i) or f_x_to_zero(right.adc_sta_reserved_clk_chains_i);
tmp.adc_sta_data_chains_i := f_x_to_zero(left.adc_sta_data_chains_i) or f_x_to_zero(right.adc_sta_data_chains_i);
tmp.adc_sta_reserved_data_chains_i := f_x_to_zero(left.adc_sta_reserved_data_chains_i) or f_x_to_zero(right.adc_sta_reserved_data_chains_i);
tmp.adc_sta_adc_pkt_size_i := f_x_to_zero(left.adc_sta_adc_pkt_size_i) or f_x_to_zero(right.adc_sta_adc_pkt_size_i);
tmp.adc_ctl_reserved_i := f_x_to_zero(left.adc_ctl_reserved_i) or f_x_to_zero(right.adc_ctl_reserved_i);
tmp.ch0_sta_val_i := f_x_to_zero(left.ch0_sta_val_i) or f_x_to_zero(right.ch0_sta_val_i);
tmp.ch0_sta_reserved_i := f_x_to_zero(left.ch0_sta_reserved_i) or f_x_to_zero(right.ch0_sta_reserved_i);
tmp.ch0_ctl_clk_chain_dly_i := f_x_to_zero(left.ch0_ctl_clk_chain_dly_i) or f_x_to_zero(right.ch0_ctl_clk_chain_dly_i);
tmp.ch0_ctl_reserved_clk_chain_dly_i := f_x_to_zero(left.ch0_ctl_reserved_clk_chain_dly_i) or f_x_to_zero(right.ch0_ctl_reserved_clk_chain_dly_i);
tmp.ch0_ctl_data_chain_dly_i := f_x_to_zero(left.ch0_ctl_data_chain_dly_i) or f_x_to_zero(right.ch0_ctl_data_chain_dly_i);
tmp.ch0_ctl_reserved_data_chain_dly_i := f_x_to_zero(left.ch0_ctl_reserved_data_chain_dly_i) or f_x_to_zero(right.ch0_ctl_reserved_data_chain_dly_i);
tmp.ch1_sta_val_i := f_x_to_zero(left.ch1_sta_val_i) or f_x_to_zero(right.ch1_sta_val_i);
tmp.ch1_sta_reserved_i := f_x_to_zero(left.ch1_sta_reserved_i) or f_x_to_zero(right.ch1_sta_reserved_i);
tmp.ch1_ctl_clk_chain_dly_i := f_x_to_zero(left.ch1_ctl_clk_chain_dly_i) or f_x_to_zero(right.ch1_ctl_clk_chain_dly_i);
tmp.ch1_ctl_reserved_clk_chain_dly_i := f_x_to_zero(left.ch1_ctl_reserved_clk_chain_dly_i) or f_x_to_zero(right.ch1_ctl_reserved_clk_chain_dly_i);
tmp.ch1_ctl_data_chain_dly_i := f_x_to_zero(left.ch1_ctl_data_chain_dly_i) or f_x_to_zero(right.ch1_ctl_data_chain_dly_i);
tmp.ch1_ctl_reserved_data_chain_dly_i := f_x_to_zero(left.ch1_ctl_reserved_data_chain_dly_i) or f_x_to_zero(right.ch1_ctl_reserved_data_chain_dly_i);
tmp.ch2_sta_val_i := f_x_to_zero(left.ch2_sta_val_i) or f_x_to_zero(right.ch2_sta_val_i);
tmp.ch2_sta_reserved_i := f_x_to_zero(left.ch2_sta_reserved_i) or f_x_to_zero(right.ch2_sta_reserved_i);
tmp.ch2_ctl_clk_chain_dly_i := f_x_to_zero(left.ch2_ctl_clk_chain_dly_i) or f_x_to_zero(right.ch2_ctl_clk_chain_dly_i);
tmp.ch2_ctl_reserved_clk_chain_dly_i := f_x_to_zero(left.ch2_ctl_reserved_clk_chain_dly_i) or f_x_to_zero(right.ch2_ctl_reserved_clk_chain_dly_i);
tmp.ch2_ctl_data_chain_dly_i := f_x_to_zero(left.ch2_ctl_data_chain_dly_i) or f_x_to_zero(right.ch2_ctl_data_chain_dly_i);
tmp.ch2_ctl_reserved_data_chain_dly_i := f_x_to_zero(left.ch2_ctl_reserved_data_chain_dly_i) or f_x_to_zero(right.ch2_ctl_reserved_data_chain_dly_i);
tmp.ch3_sta_val_i := f_x_to_zero(left.ch3_sta_val_i) or f_x_to_zero(right.ch3_sta_val_i);
tmp.ch3_sta_reserved_i := f_x_to_zero(left.ch3_sta_reserved_i) or f_x_to_zero(right.ch3_sta_reserved_i);
tmp.ch3_ctl_clk_chain_dly_i := f_x_to_zero(left.ch3_ctl_clk_chain_dly_i) or f_x_to_zero(right.ch3_ctl_clk_chain_dly_i);
tmp.ch3_ctl_reserved_clk_chain_dly_i := f_x_to_zero(left.ch3_ctl_reserved_clk_chain_dly_i) or f_x_to_zero(right.ch3_ctl_reserved_clk_chain_dly_i);
tmp.ch3_ctl_data_chain_dly_i := f_x_to_zero(left.ch3_ctl_data_chain_dly_i) or f_x_to_zero(right.ch3_ctl_data_chain_dly_i);
tmp.ch3_ctl_reserved_data_chain_dly_i := f_x_to_zero(left.ch3_ctl_reserved_data_chain_dly_i) or f_x_to_zero(right.ch3_ctl_reserved_data_chain_dly_i);
return tmp;
end function;
end package body;

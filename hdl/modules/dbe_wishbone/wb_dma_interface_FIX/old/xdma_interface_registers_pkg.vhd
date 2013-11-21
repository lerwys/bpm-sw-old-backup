---------------------------------------------------------------------------------------
-- Title          : Wishbone slave core for Wishbone DMA Streaming Interface
---------------------------------------------------------------------------------------
-- File           : xdma_interface_registers_pkg.vhd
-- Author         : auto-generated by wbgen2 from xdma_interface_wb.wb
-- Created        : Thu Sep 27 15:39:56 2012
-- Standard       : VHDL'87
---------------------------------------------------------------------------------------
-- THIS FILE WAS GENERATED BY wbgen2 FROM SOURCE FILE xdma_interface_wb.wb
-- DO NOT HAND-EDIT UNLESS IT'S ABSOLUTELY NECESSARY!
---------------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.wbgen2_pkg.all;

package dma_iface_wbgen2_pkg is
  
  
  -- Input registers (user design -> WB slave)
  
  type t_dma_iface_in_registers is record
    ctl_done_i                               : std_logic;
    ctl_ovf_i                                : std_logic;
    fifo_c2b_wr_req_i                        : std_logic;
    fifo_c2b_data_i                          : std_logic_vector(31 downto 0);
    fifo_c2b_last_i                          : std_logic;
    fifo_b2c_rd_req_i                        : std_logic;
    end record;
  
  constant c_dma_iface_in_registers_init_value: t_dma_iface_in_registers := (
    ctl_done_i => '0',
    ctl_ovf_i => '0',
    fifo_c2b_wr_req_i => '0',
    fifo_c2b_data_i => (others => '0'),
    fifo_c2b_last_i => '0',
    fifo_b2c_rd_req_i => '0'
    );
    
    -- Output registers (WB slave -> user design)
    
    type t_dma_iface_out_registers is record
      ctl_start_o                              : std_logic;
      tr_cntr_o                                : std_logic_vector(31 downto 0);
      fifo_c2b_wr_full_o                       : std_logic;
      fifo_c2b_wr_empty_o                      : std_logic;
      fifo_c2b_wr_usedw_o                      : std_logic_vector(7 downto 0);
      fifo_b2c_rd_full_o                       : std_logic;
      fifo_b2c_rd_empty_o                      : std_logic;
      fifo_b2c_rd_usedw_o                      : std_logic_vector(7 downto 0);
      fifo_b2c_data_o                          : std_logic_vector(31 downto 0);
      end record;
    
    constant c_dma_iface_out_registers_init_value: t_dma_iface_out_registers := (
      ctl_start_o => '0',
      tr_cntr_o => (others => '0'),
      fifo_c2b_wr_full_o => '0',
      fifo_c2b_wr_empty_o => '0',
      fifo_c2b_wr_usedw_o => (others => '0'),
      fifo_b2c_rd_full_o => '0',
      fifo_b2c_rd_empty_o => '0',
      fifo_b2c_rd_usedw_o => (others => '0'),
      fifo_b2c_data_o => (others => '0')
      );
    function "or" (left, right: t_dma_iface_in_registers) return t_dma_iface_in_registers;
    function f_x_to_zero (x:std_logic) return std_logic;
    function f_x_to_zero (x:std_logic_vector) return std_logic_vector;
end package;

package body dma_iface_wbgen2_pkg is
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
function "or" (left, right: t_dma_iface_in_registers) return t_dma_iface_in_registers is
variable tmp: t_dma_iface_in_registers;
begin
tmp.ctl_done_i := f_x_to_zero(left.ctl_done_i) or f_x_to_zero(right.ctl_done_i);
tmp.ctl_ovf_i := f_x_to_zero(left.ctl_ovf_i) or f_x_to_zero(right.ctl_ovf_i);
tmp.fifo_c2b_wr_req_i := f_x_to_zero(left.fifo_c2b_wr_req_i) or f_x_to_zero(right.fifo_c2b_wr_req_i);
tmp.fifo_c2b_data_i := f_x_to_zero(left.fifo_c2b_data_i) or f_x_to_zero(right.fifo_c2b_data_i);
tmp.fifo_c2b_last_i := f_x_to_zero(left.fifo_c2b_last_i) or f_x_to_zero(right.fifo_c2b_last_i);
tmp.fifo_b2c_rd_req_i := f_x_to_zero(left.fifo_b2c_rd_req_i) or f_x_to_zero(right.fifo_b2c_rd_req_i);
return tmp;
end function;
end package body;
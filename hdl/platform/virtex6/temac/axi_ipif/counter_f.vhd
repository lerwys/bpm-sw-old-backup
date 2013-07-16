-- Project    : Xilinx LogiCORE Virtex-6 Embedded Tri-Mode Ethernet MAC
-- File       : counter_f.vhd
-- Version    : 2.2
-------------------------------------------------------------------------------
--
-- (c) Copyright 2010 Xilinx, Inc. All rights reserved.
--
-- This file contains confidential and proprietary information
-- of Xilinx, Inc. and is protected under U.S. and
-- international copyright and other intellectual property
-- laws.
--
-- DISCLAIMER
-- This disclaimer is not a license and does not grant any
-- rights to the materials distributed herewith. Except as
-- otherwise provided in a valid license issued to you by
-- Xilinx, and to the maximum extent permitted by applicable
-- law: (1) THESE MATERIALS ARE MADE AVAILABLE "AS IS" AND
-- WITH ALL FAULTS, AND XILINX HEREBY DISCLAIMS ALL WARRANTIES
-- AND CONDITIONS, EXPRESS, IMPLIED, OR STATUTORY, INCLUDING
-- BUT NOT LIMITED TO WARRANTIES OF MERCHANTABILITY, NON-
-- INFRINGEMENT, OR FITNESS FOR ANY PARTICULAR PURPOSE; and
-- (2) Xilinx shall not be liable (whether in contract or tort,
-- including negligence, or under any other theory of
-- liability) for any loss or damage of any kind or nature
-- related to, arising under or in connection with these
-- materials, including for any direct, or any indirect,
-- special, incidental, or consequential loss or damage
-- (including loss of data, profits, goodwill, or any type of
-- loss or damage suffered as a result of any action brought
-- by a third party) even if such damage or loss was
-- reasonably foreseeable or Xilinx had been advised of the
-- possibility of the same.
--
-- CRITICAL APPLICATIONS
-- Xilinx products are not designed or intended to be fail-
-- safe, or for use in any application requiring fail-safe
-- performance, such as life-support or safety devices or
-- systems, Class III medical devices, nuclear facilities,
-- applications related to the deployment of airbags, or any
-- other applications that could lead to death, personal
-- injury, or severe property or environmental damage
-- (individually and collectively, "Critical
-- Applications"). Customer assumes the sole risk and
-- liability of any use of Xilinx products in Critical
-- Applications, subject only to applicable laws and
-- regulations governing limitations on product liability.
--
-- THIS COPYRIGHT NOTICE AND DISCLAIMER MUST BE RETAINED AS
-- PART OF THIS FILE AT ALL TIMES.
--
-------------------------------------------------------------------------------
-- Filename:        counter_f.vhd
--
-- Description:     Implements a parameterizable N-bit counter_f
--                      Up/Down Counter
--                      Count Enable
--                      Parallel Load
--                      Synchronous Reset
--                      The structural implementation has incremental cost
--                      of one LUT per bit.
--                      Precedence of operations when simultaneous:
--                        reset, load, count
--
--                  A default inferred-RTL implementation is provided and
--                  is used if the user explicitly specifies C_FAMILY=nofamily
--                  or ommits C_FAMILY (allowing it to default to nofamily).
--                  The default implementation is also used
--                  if needed primitives are not available in FPGAs of the
--                  type given by C_FAMILY.
--
-- VHDL-Standard:   VHDL'93
-------------------------------------------------------------------------------
-- Structure:
--                  counter_f.vhd
--
-------------------------------------------------------------------------------

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.unsigned;
use IEEE.numeric_std."+";
use IEEE.numeric_std."-";

library unisim;
use unisim.all;

-----------------------------------------------------------------------------
-- Entity section
-----------------------------------------------------------------------------

entity counter_f is
    generic(
            C_NUM_BITS : integer := 9;
            C_FAMILY   : string := "nofamily"
           );

    port(
         Clk           : in  std_logic;
         Rst           : in  std_logic;
         Load_In       : in  std_logic_vector(C_NUM_BITS - 1 downto 0);
         Count_Enable  : in  std_logic;
         Count_Load    : in  std_logic;
         Count_Down    : in  std_logic;
         Count_Out     : out std_logic_vector(C_NUM_BITS - 1 downto 0);
         Carry_Out     : out std_logic
        );
end entity counter_f;

-----------------------------------------------------------------------------
-- Architecture section
-----------------------------------------------------------------------------

architecture imp of counter_f is

    signal icount_out    : unsigned(C_NUM_BITS downto 0);
    signal icount_out_x  : unsigned(C_NUM_BITS downto 0);
    signal load_in_x     : unsigned(C_NUM_BITS downto 0);

---------------------------------------------------------------------
-- Begin architecture
---------------------------------------------------------------------
begin

   load_in_x    <= unsigned('0' & Load_In);

   -- Mask out carry position to retain legacy self-clear on next enable.
 --   icount_out_x <= ('0' & icount_out(C_NUM_BITS-1 downto 0)); -- Echeck WA
    icount_out_x <= unsigned('0' & std_logic_vector(icount_out(C_NUM_BITS-1 downto 0)));

   -----------------------------------------------------------------
   -- Process to generate counter with - synchronous reset, load,
   -- counter enable, count down / up features.
   -----------------------------------------------------------------
   CNTR_PROC : process(Clk)
   begin
       if Clk'event and Clk = '1' then
           if Rst = '1' then
               icount_out <= (others => '0');
           elsif Count_Load = '1' then
               icount_out <= load_in_x;
           elsif Count_Down = '1'  and Count_Enable = '1' then
               icount_out <= icount_out_x - 1;
           elsif Count_Enable = '1' then
               icount_out <= icount_out_x + 1;
           end if;
       end if;
   end process CNTR_PROC;

   Carry_Out <= icount_out(C_NUM_BITS);
   Count_Out <= std_logic_vector(icount_out(C_NUM_BITS-1 downto 0));


end architecture imp;
---------------------------------------------------------------
-- End of file counter_f.vhd
---------------------------------------------------------------

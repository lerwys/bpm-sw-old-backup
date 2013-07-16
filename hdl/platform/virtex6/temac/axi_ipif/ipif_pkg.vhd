-- Project    : Xilinx LogiCORE Virtex-6 Embedded Tri-Mode Ethernet MAC
-- File       : ipif_pkg.vhd
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
-- Filename:        ipif_pkg.vhd
-- Version:         Intital
-- Description:     This file contains the constants and functions used in the
--                  ipif common library components.
--
-------------------------------------------------------------------------------
-- Structure:
--
-------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
-- need conversion function to convert reals/integers to std logic vectors
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;


package ipif_pkg is


-------------------------------------------------------------------------------
-- Type Declarations
-------------------------------------------------------------------------------
type    SLV32_ARRAY_TYPE is array (natural range <>) of std_logic_vector(0 to 31);
subtype SLV64_TYPE is std_logic_vector(0 to 63);
type    SLV64_ARRAY_TYPE is array (natural range <>) of SLV64_TYPE;
type    INTEGER_ARRAY_TYPE is array (natural range <>) of integer;

-------------------------------------------------------------------------------
-- Component Definitions
-------------------------------------------------------------------------------

 component counter_f
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
 end component;

 component pselect_f

   generic (
     C_AB     : integer := 9;
     C_AW     : integer := 32;
     C_BAR    : std_logic_vector;
     C_FAMILY : string := "nofamily"
     );
   port (
     A        : in   std_logic_vector(0 to C_AW-1);
     AValid   : in   std_logic;
     CS       : out  std_logic
     );

 end component;


-------------------------------------------------------------------------------
-- Function and Procedure Declarations
-------------------------------------------------------------------------------
function calc_num_ce (ce_num_array : INTEGER_ARRAY_TYPE) return integer;
function calc_start_ce_index (ce_num_array : INTEGER_ARRAY_TYPE;
                              index        : integer) return integer;
function max2 (num1, num2 : integer) return integer;
function clog2(x : positive) return natural;

end ipif_pkg;


package body ipif_pkg is



-------------------------------------------------------------------------------
-- Function Definitions
-------------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- Function calc_num_ce
--
-- This function is used to process the array specifying the number of Chip
-- Enables required for a Base Address specification. The array is input to
-- the function and an integer is returned reflecting the total number of
-- Chip Enables required for the CE, RdCE, and WrCE Buses
-----------------------------------------------------------------------------
  function calc_num_ce (ce_num_array : INTEGER_ARRAY_TYPE) return integer is

     Variable ce_num_sum : integer := 0;

  begin

    for i in 0 to (ce_num_array'length)-1 loop
        ce_num_sum := ce_num_sum + ce_num_array(i);
    End loop;

    return(ce_num_sum);

  end function calc_num_ce;

-----------------------------------------------------------------------------
-- Function calc_start_ce_index
--
-- This function is used to process the array specifying the number of Chip
-- Enables required for a Base Address specification. The CE Size array is
-- input to the function and an integer index representing the index of the
-- target module in the ce_num_array. An integer is returned reflecting the
-- starting index of the assigned Chip Enables within the CE, RdCE, and
-- WrCE Buses.
-----------------------------------------------------------------------------
 function calc_start_ce_index (ce_num_array : INTEGER_ARRAY_TYPE;
                               index        : integer) return integer is

    Variable ce_num_sum : integer := 0;

 begin
   If (index = 0) Then
     ce_num_sum := 0;
   else
      for i in 0 to index-1 loop
          ce_num_sum := ce_num_sum + ce_num_array(i);
      End loop;
   End if;

   return(ce_num_sum);

 end function calc_start_ce_index;

 -------------------------------------------------------------------------------
 -- Function max2
 --
 -- This function returns the greater of two numbers.
 -------------------------------------------------------------------------------
 function max2 (num1, num2 : integer) return integer is
 begin
     if num1 >= num2 then
         return num1;
     else
         return num2;
     end if;
 end function max2;

 --------------------------------------------------------------------------------
 -- Function clog2 - returns the integer ceiling of the base 2 logarithm of x,
 --                  i.e., the least integer greater than or equal to log2(x).
 --------------------------------------------------------------------------------
 function clog2(x : positive) return natural is
   variable r  : natural := 0;
   variable rp : natural := 1; -- rp tracks the value 2**r
 begin
   while rp < x loop -- Termination condition T: x <= 2**r
     -- Loop invariant L: 2**(r-1) < x
     r := r + 1;
     if rp > integer'high - rp then exit; end if;  -- If doubling rp overflows
       -- the integer range, the doubled value would exceed x, so safe to exit.
     rp := rp + rp;
   end loop;
   -- L and T  <->  2**(r-1) < x <= 2**r  <->  (r-1) < log2(x) <= r
   return r; --
 end clog2;


end package body ipif_pkg;

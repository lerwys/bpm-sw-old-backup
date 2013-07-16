-- Project    : Xilinx LogiCORE Virtex-6 Embedded Tri-Mode Ethernet MAC
-- File       : axi4_lite_ipif_wrapper.vhd
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
-- Filename:        axi4_lite_ipif_wrapper.vhd
-- Version:         v1.00.a
-- Description:     A simple wrapper to convert between aloowed generics etc
--                 in verilog and those used in the axi_ipif block
-------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.numeric_std.all;
use ieee.std_logic_misc.all;

library work;
use work.ipif_pkg.all;


entity axi4_lite_ipif_wrapper is
    generic (
      C_BASE_ADDRESS        : std_logic_vector(31 downto 0) := X"00000000";
      C_HIGH_ADDRESS        : std_logic_vector(31 downto 0) := X"000007FF"
      );
    port (

        --System signals
      S_AXI_ACLK            : in  std_logic;
      S_AXI_ARESETN         : in  std_logic;
      S_AXI_AWADDR          : in  std_logic_vector(31 downto 0);
      S_AXI_AWVALID         : in  std_logic;
      S_AXI_AWREADY         : out std_logic;
      S_AXI_WDATA           : in  std_logic_vector(31 downto 0);
      S_AXI_WVALID          : in  std_logic;
      S_AXI_WREADY          : out std_logic;
      S_AXI_BRESP           : out std_logic_vector(1 downto 0);
      S_AXI_BVALID          : out std_logic;
      S_AXI_BREADY          : in  std_logic;
      S_AXI_ARADDR          : in  std_logic_vector(31 downto 0);
      S_AXI_ARVALID         : in  std_logic;
      S_AXI_ARREADY         : out std_logic;
      S_AXI_RDATA           : out std_logic_vector(31 downto 0);
      S_AXI_RRESP           : out std_logic_vector(1 downto 0);
      S_AXI_RVALID          : out std_logic;
      S_AXI_RREADY          : in  std_logic;
      -- Controls to the IP/IPIF modules
      Bus2IP_Clk            : out std_logic;
      Bus2IP_Reset          : out std_logic;
      Bus2IP_Addr           : out std_logic_vector(31 downto 0);
      Bus2IP_CS             : out std_logic;
      Bus2IP_RdCE           : out std_logic;
      Bus2IP_WrCE           : out std_logic;
      Bus2IP_Data           : out std_logic_vector(31 downto 0);
      IP2Bus_Data           : in  std_logic_vector(31 downto 0);
      IP2Bus_WrAck          : in  std_logic;
      IP2Bus_RdAck          : in  std_logic;
      IP2Bus_Error          : in  std_logic

       );

end axi4_lite_ipif_wrapper;

-------------------------------------------------------------------------------
-- Architecture
-------------------------------------------------------------------------------

architecture rtl of axi4_lite_ipif_wrapper is

   component axi_lite_ipif
    generic (

      C_S_AXI_DATA_WIDTH    : integer  range 32 to 32   := 32;
      C_S_AXI_ADDR_WIDTH    : integer                   := 32;
      C_S_AXI_MIN_SIZE      : std_logic_vector(31 downto 0):= X"000001FF";
      C_USE_WSTRB           : integer := 0;
      C_DPHASE_TIMEOUT      : integer range 0 to 256   := 16;
      C_ARD_ADDR_RANGE_ARRAY: SLV64_ARRAY_TYPE :=  -- not used
         (
           X"0000_0000_7000_0000", -- IP user0 base address
           X"0000_0000_7000_00FF", -- IP user0 high address
           X"0000_0000_7000_0100", -- IP user1 base address
           X"0000_0000_7000_01FF"  -- IP user1 high address
         );

      C_ARD_NUM_CE_ARRAY    : INTEGER_ARRAY_TYPE := -- not used
         (
           4,         -- User0 CE Number
           12          -- User1 CE Number
         );
      C_FAMILY              : string  := "virtex6"
           );
    port (

        --System signals
      S_AXI_ACLK            : in  std_logic;
      S_AXI_ARESETN         : in  std_logic;
      S_AXI_AWADDR          : in  std_logic_vector
                              (C_S_AXI_ADDR_WIDTH-1 downto 0);
      S_AXI_AWVALID         : in  std_logic;
      S_AXI_AWREADY         : out std_logic;
      S_AXI_WDATA           : in  std_logic_vector
                              (C_S_AXI_DATA_WIDTH-1 downto 0);
      S_AXI_WSTRB           : in  std_logic_vector
                              ((C_S_AXI_DATA_WIDTH/8)-1 downto 0);
      S_AXI_WVALID          : in  std_logic;
      S_AXI_WREADY          : out std_logic;
      S_AXI_BRESP           : out std_logic_vector(1 downto 0);
      S_AXI_BVALID          : out std_logic;
      S_AXI_BREADY          : in  std_logic;
      S_AXI_ARADDR          : in  std_logic_vector
                              (C_S_AXI_ADDR_WIDTH-1 downto 0);
      S_AXI_ARVALID         : in  std_logic;
      S_AXI_ARREADY         : out std_logic;
      S_AXI_RDATA           : out std_logic_vector
                              (C_S_AXI_DATA_WIDTH-1 downto 0);
      S_AXI_RRESP           : out std_logic_vector(1 downto 0);
      S_AXI_RVALID          : out std_logic;
      S_AXI_RREADY          : in  std_logic;
      -- Controls to the IP/IPIF modules
      Bus2IP_Clk            : out std_logic;
      Bus2IP_Resetn         : out std_logic;
      Bus2IP_Addr           : out std_logic_vector
                              ((C_S_AXI_ADDR_WIDTH-1) downto 0);
      Bus2IP_RNW            : out std_logic;
      Bus2IP_BE             : out std_logic_vector
                              (((C_S_AXI_DATA_WIDTH/8)-1) downto 0);
      Bus2IP_CS             : out std_logic_vector
                              (((C_ARD_ADDR_RANGE_ARRAY'LENGTH)/2-1) downto 0);
      Bus2IP_RdCE           : out std_logic_vector
                              ((calc_num_ce(C_ARD_NUM_CE_ARRAY)-1) downto 0);
      Bus2IP_WrCE           : out std_logic_vector
                              ((calc_num_ce(C_ARD_NUM_CE_ARRAY)-1) downto 0);
      Bus2IP_Data           : out std_logic_vector
                              ((C_S_AXI_DATA_WIDTH-1) downto 0);
      IP2Bus_Data           : in  std_logic_vector
                              ((C_S_AXI_DATA_WIDTH-1) downto 0);
      IP2Bus_WrAck          : in  std_logic;
      IP2Bus_RdAck          : in  std_logic;
      IP2Bus_Error          : in  std_logic

       );
   end component;

-- Stats 200-3FF, MAC 400-5FF, INTC 600-6FF, ADR 700-7FF
constant C_BASE_ADDRESS_TEMAC1 : std_logic_vector(63 downto 0):= (X"00000000" & (C_BASE_ADDRESS));
constant C_HIGH_ADDRESS_TEMAC1 : std_logic_vector(63 downto 0):= (X"00000000" & (C_HIGH_ADDRESS));
constant C_BASE_ADDRESS_TEMAC2 : std_logic_vector(63 downto 0):= (X"00000000" & (C_BASE_ADDRESS));
constant C_HIGH_ADDRESS_TEMAC2 : std_logic_vector(63 downto 0):= (X"00000000" & (C_HIGH_ADDRESS));

signal bus2ip_clk_int       : std_logic;
signal bus2ip_resetn_int    : std_logic;
signal bus2ip_addr_int      : std_logic_vector(31 downto 0);
signal bus2ip_cs_int        : std_logic_vector(1 downto 0);
signal bus2ip_rdce_int      : std_logic_vector(1 downto 0);
signal local_wrack          : std_logic;
signal local_rdack          : std_logic;
signal cs_edge_reg          : std_logic;
signal ip2bus_rdack_comb    : std_logic;
signal ip2bus_wrack_comb    : std_logic;
signal bus2ip_wrce_int      : std_logic_vector(1 downto 0);

-------------------------------------------------------------------------------
-- Begin architecture logic
-------------------------------------------------------------------------------
begin

   Bus2IP_Clk    <=  bus2ip_clk_int;
   Bus2IP_Reset  <=  not bus2ip_resetn_int;
   Bus2IP_Addr   <=  bus2ip_addr_int;
   Bus2IP_CS     <=  bus2ip_cs_int(1);
   Bus2IP_RdCE   <=  bus2ip_rdce_int(1);
   Bus2IP_WrCE   <=  bus2ip_wrce_int(1);


   axi_lite_top : axi_lite_ipif
    generic map (

      C_S_AXI_MIN_SIZE      => X"000007FF",
      C_ARD_ADDR_RANGE_ARRAY=> (C_BASE_ADDRESS_TEMAC1, C_HIGH_ADDRESS_TEMAC1,
                                C_BASE_ADDRESS_TEMAC2, C_HIGH_ADDRESS_TEMAC2),
      C_ARD_NUM_CE_ARRAY    => (1,1),
      C_FAMILY              => "virtex6"
           )
    port map (

        --System signals
      S_AXI_ACLK            => S_AXI_ACLK,
      S_AXI_ARESETN         => S_AXI_ARESETN,
      S_AXI_AWADDR          => S_AXI_AWADDR,

      S_AXI_AWVALID         => S_AXI_AWVALID,
      S_AXI_AWREADY         => S_AXI_AWREADY,
      S_AXI_WDATA           => S_AXI_WDATA,

      S_AXI_WSTRB           => "0000",

      S_AXI_WVALID          => S_AXI_WVALID,
      S_AXI_WREADY          => S_AXI_WREADY,
      S_AXI_BRESP           => S_AXI_BRESP,
      S_AXI_BVALID          => S_AXI_BVALID,
      S_AXI_BREADY          => S_AXI_BREADY,
      S_AXI_ARADDR          => S_AXI_ARADDR,

      S_AXI_ARVALID         => S_AXI_ARVALID,
      S_AXI_ARREADY         => S_AXI_ARREADY,
      S_AXI_RDATA           => S_AXI_RDATA,

      S_AXI_RRESP           => S_AXI_RRESP,
      S_AXI_RVALID          => S_AXI_RVALID,
      S_AXI_RREADY          => S_AXI_RREADY,
      -- Controls to the IP/IPIF
      Bus2IP_Clk            => bus2ip_clk_int,
      Bus2IP_Resetn         => bus2ip_resetn_int,
      Bus2IP_Addr           => bus2ip_addr_int,

      Bus2IP_RNW            => open,
      Bus2IP_BE             => open,

      Bus2IP_CS             => bus2ip_cs_int,

      Bus2IP_RdCE           => bus2ip_rdce_int,

      Bus2IP_WrCE           => bus2ip_wrce_int,

      Bus2IP_Data           => Bus2IP_Data,

      IP2Bus_Data           => IP2Bus_Data,

      IP2Bus_WrAck          => ip2bus_wrack_comb,
      IP2Bus_RdAck          => ip2bus_rdack_comb,
      IP2Bus_Error          => IP2Bus_Error

    );

  local_ack_gen: process (bus2ip_clk_int)
  begin
     if bus2ip_clk_int'event and bus2ip_clk_int = '1' then
       if bus2ip_resetn_int = '0' then
          local_wrack <= '0';
          local_rdack <= '0';
          cs_edge_reg <= '0';
       else
          cs_edge_reg <= bus2ip_cs_int(1);
          if bus2ip_addr_int(31 downto 9) = "000000000000000000000000" and
             (bus2ip_cs_int(1) = '1' and cs_edge_reg = '0') then
             if bus2ip_rdce_int(1) = '1' then
                local_rdack <= '1';
             end if;
             if bus2ip_wrce_int(1) = '1' then
                local_wrack <= '1';
             end if;
          else
             local_wrack <= '0';
             local_rdack <= '0';
          end if;
       end if;
     end if;
  end process local_ack_gen;


  ip2bus_rdack_comb <= local_rdack or IP2Bus_RdAck;
  ip2bus_wrack_comb <= local_wrack or IP2Bus_WrAck;

end rtl;

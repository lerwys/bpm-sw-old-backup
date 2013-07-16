-- Project    : Xilinx LogiCORE Virtex-6 Embedded Tri-Mode Ethernet MAC
-- File       : slave_attachment.vhd
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
-- Filename:        slave_attachment.vhd
-- Version:         v1.00.a
-- Description:     AXI slave attachment supporting single transfers
-------------------------------------------------------------------------------
-- Structure:   This section shows the hierarchical structure of axi_lite_ipif.
--
--              --axi_lite_ipif.vhd
--                    --slave_attachment.vhd
--                       --address_decoder.vhd
-------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_unsigned.all;
use ieee.std_logic_misc.all;

library work;
use work.ipif_pkg.all;
-------------------------------------------------------------------------------
--                     Definition of Generics
-------------------------------------------------------------------------------
-- C_IPIF_ABUS_WIDTH     -- IPIF Address bus width
-- C_IPIF_DBUS_WIDTH     -- IPIF Data Bus width
-- C_S_AXI_MIN_SIZE      -- Minimum address range of the IP
-- C_USE_WSTRB           -- Use write strobs or not
-- C_DPHASE_TIMEOUT      -- Data phase time out counter
-- C_ARD_ADDR_RANGE_ARRAY-- Base /High Address Pair for each Address Range
-- C_ARD_NUM_CE_ARRAY    -- Desired number of chip enables for an address range
-- C_FAMILY              -- Target FPGA family
-------------------------------------------------------------------------------
--                  Definition of Ports
-------------------------------------------------------------------------------
-- S_AXI_ACLK            -- AXI Clock
-- S_AXI_ARESET          -- AXI Reset
-- S_AXI_AWADDR          -- AXI Write address
-- S_AXI_AWVALID         -- Write address valid
-- S_AXI_AWREADY         -- Write address ready
-- S_AXI_WDATA           -- Write data
-- S_AXI_WSTRB           -- Write strobes
-- S_AXI_WVALID          -- Write valid
-- S_AXI_WREADY          -- Write ready
-- S_AXI_BRESP           -- Write response
-- S_AXI_BVALID          -- Write response valid
-- S_AXI_BREADY          -- Response ready
-- S_AXI_ARADDR          -- Read address
-- S_AXI_ARVALID         -- Read address valid
-- S_AXI_ARREADY         -- Read address ready
-- S_AXI_RDATA           -- Read data
-- S_AXI_RRESP           -- Read response
-- S_AXI_RVALID          -- Read valid
-- S_AXI_RREADY          -- Read ready
-- Bus2IP_Clk            -- Synchronization clock provided to User IP
-- Bus2IP_Reset          -- Active high reset for use by the User IP
-- Bus2IP_Addr           -- Desired address of read or write operation
-- Bus2IP_RNW            -- Read or write indicator for the transaction
-- Bus2IP_BE             -- Byte enables for the data bus
-- Bus2IP_CS             -- Chip select for the transcations
-- Bus2IP_RdCE           -- Chip enables for the read
-- Bus2IP_WrCE           -- Chip enables for the write
-- Bus2IP_Data           -- Write data bus to the User IP
-- IP2Bus_Data           -- Input Read Data bus from the User IP
-- IP2Bus_WrAck          -- Active high Write Data qualifier from the IP
-- IP2Bus_RdAck          -- Active high Read Data qualifier from the IP
-- IP2Bus_Error          -- Error signal from the IP
-------------------------------------------------------------------------------

entity slave_attachment is
  generic (

    C_ARD_ADDR_RANGE_ARRAY: SLV64_ARRAY_TYPE :=
       (
        X"0000_0000_7000_0000", -- IP user0 base address
        X"0000_0000_7000_00FF", -- IP user0 high address
        X"0000_0000_7000_0100", -- IP user1 base address
        X"0000_0000_7000_01FF"  -- IP user1 high address
       );
    C_ARD_NUM_CE_ARRAY  : INTEGER_ARRAY_TYPE :=
       (
        1,         -- User0 CE Number
        8          -- User1 CE Number
       );
    C_IPIF_ABUS_WIDTH   : integer := 32;
    C_IPIF_DBUS_WIDTH   : integer := 32;
    C_S_AXI_MIN_SIZE    : std_logic_vector(31 downto 0):= X"000001FF";
    C_USE_WSTRB         : integer := 0;
    C_DPHASE_TIMEOUT    : integer:= 16;
    C_FAMILY            : string  := "virtex6"
        );
  port(
        -- AXI signals
    S_AXI_ACLK          : in  std_logic;
    S_AXI_ARESETN       : in  std_logic;
    S_AXI_AWADDR        : in  std_logic_vector
                          (C_IPIF_ABUS_WIDTH-1 downto 0);
    S_AXI_AWVALID       : in  std_logic;
    S_AXI_AWREADY       : out std_logic;
    S_AXI_WDATA         : in  std_logic_vector
                          (C_IPIF_DBUS_WIDTH-1 downto 0);
    S_AXI_WSTRB         : in  std_logic_vector
                          ((C_IPIF_DBUS_WIDTH/8)-1 downto 0);
    S_AXI_WVALID        : in  std_logic;
    S_AXI_WREADY        : out std_logic;
    S_AXI_BRESP         : out std_logic_vector(1 downto 0);
    S_AXI_BVALID        : out std_logic;
    S_AXI_BREADY        : in  std_logic;
    S_AXI_ARADDR        : in  std_logic_vector
                          (C_IPIF_ABUS_WIDTH-1 downto 0);
    S_AXI_ARVALID       : in  std_logic;
    S_AXI_ARREADY       : out std_logic;
    S_AXI_RDATA         : out std_logic_vector
                          (C_IPIF_DBUS_WIDTH-1 downto 0);
    S_AXI_RRESP         : out std_logic_vector(1 downto 0);
    S_AXI_RVALID        : out std_logic;
    S_AXI_RREADY        : in  std_logic;
    -- Controls to the IP/IPIF modules
    Bus2IP_Clk          : out std_logic;
    Bus2IP_Resetn       : out std_logic;
    Bus2IP_Addr         : out std_logic_vector
                          (C_IPIF_ABUS_WIDTH-1 downto 0);
    Bus2IP_RNW          : out std_logic;
    Bus2IP_BE           : out std_logic_vector
                          (((C_IPIF_DBUS_WIDTH/8) - 1) downto 0);
    Bus2IP_CS           : out std_logic_vector
                          (((C_ARD_ADDR_RANGE_ARRAY'LENGTH)/2 - 1) downto 0);
    Bus2IP_RdCE         : out std_logic_vector
                          ((calc_num_ce(C_ARD_NUM_CE_ARRAY) - 1) downto 0);
    Bus2IP_WrCE         : out std_logic_vector
                          ((calc_num_ce(C_ARD_NUM_CE_ARRAY) - 1) downto 0);
    Bus2IP_Data         : out std_logic_vector
                          ((C_IPIF_DBUS_WIDTH-1) downto 0);
    IP2Bus_Data         : in  std_logic_vector
                          ((C_IPIF_DBUS_WIDTH-1) downto 0);
    IP2Bus_WrAck        : in  std_logic;
    IP2Bus_RdAck        : in  std_logic;
    IP2Bus_Error        : in  std_logic
    );
end entity slave_attachment;

-------------------------------------------------------------------------------
architecture rtl of slave_attachment is

component address_decoder
    generic (
        C_BUS_AWIDTH          : integer := 32;
        C_S_AXI_MIN_SIZE      : std_logic_vector(0 to 31) := X"000001FF";
        C_ARD_ADDR_RANGE_ARRAY: SLV64_ARRAY_TYPE :=
            (
             X"0000_0000_1000_0000", --  IP user0 base address
             X"0000_0000_1000_01FF", --  IP user0 high address
             X"0000_0000_1000_0200", --  IP user1 base address
             X"0000_0000_1000_02FF"  --  IP user1 high address
            );
        C_ARD_NUM_CE_ARRAY  : INTEGER_ARRAY_TYPE :=
            (
             8,     -- User0 CE Number
             1      -- User1 CE Number
            );
        C_FAMILY            : string  := "virtex6"
    );
  port (
        Bus_clk             : in  std_logic;
        Bus_rst             : in  std_logic;

        -- PLB Interface signals
        Address_In_Erly     : in  std_logic_vector(0 to C_BUS_AWIDTH-1);
        Address_Valid_Erly  : in  std_logic;
        Bus_RNW             : in  std_logic;
        Bus_RNW_Erly        : in  std_logic;

        -- Registering control signals
        CS_CE_ld_enable     : in  std_logic;
        Clear_CS_CE_Reg     : in  std_logic;
        RW_CE_ld_enable     : in  std_logic;
        CS_for_gaps         : out std_logic;
        -- Decode output signals
        CS_Out              : out std_logic_vector
                                (0 to ((C_ARD_ADDR_RANGE_ARRAY'LENGTH)/2)-1);
        RdCE_Out            : out std_logic_vector
                                (0 to calc_num_ce(C_ARD_NUM_CE_ARRAY)-1);
        WrCE_Out            : out std_logic_vector
                                (0 to calc_num_ce(C_ARD_NUM_CE_ARRAY)-1)
    );
end component;

-------------------------------------------------------------------------------
-- Function Declarations
-------------------------------------------------------------------------------
function Get_Addr_Bits (y : std_logic_vector(31 downto 0)) return integer is
    begin
        for i in 31 downto 0 loop
            if y(i)='1' then
               return (i);
            end if;
        end loop;
end function Get_Addr_Bits;
-------------------------------------------------------------------------------
-- Constant Declarations
-------------------------------------------------------------------------------
constant CS_BUS_SIZE          : integer := C_ARD_ADDR_RANGE_ARRAY'length/2;
constant CE_BUS_SIZE          : integer := calc_num_ce(C_ARD_NUM_CE_ARRAY);
constant C_INCLUDE_DPHASE_TIMER: integer := C_DPHASE_TIMEOUT;
constant AXI_RESP_OK          : std_logic_vector(1 downto 0) := "00";
constant AXI_RESP_SLVERR      : std_logic_vector(1 downto 0) := "10";
constant AXI_RESP_DECERR      : std_logic_vector(1 downto 0) := "11";
constant C_ADDR_DECODE_BITS   : integer := Get_Addr_Bits(C_S_AXI_MIN_SIZE);
constant C_NUM_DECODE_BITS    : integer := C_ADDR_DECODE_BITS +1;
constant ZEROS                : std_logic_vector((C_IPIF_ABUS_WIDTH-1) downto
                               (C_ADDR_DECODE_BITS+1)) := (others=>'0');
-------------------------------------------------------------------------------
-- Signal and Type Declarations
-------------------------------------------------------------------------------
signal s_axi_awready_i        : std_logic;
signal s_axi_wready_i         : std_logic;
signal s_axi_bresp_i          : std_logic_vector(1 downto 0);
signal s_axi_bvalid_i         : std_logic;
signal s_axi_arready_i        : std_logic;
signal s_axi_rdata_i          : std_logic_vector(C_IPIF_DBUS_WIDTH-1 downto 0);
signal s_axi_rresp_i          : std_logic_vector(1 downto 0);
signal s_axi_rvalid_i         : std_logic;
signal s_axi_awready_reg      : std_logic;
signal s_axi_wready_reg       : std_logic;
signal s_axi_bresp_reg        : std_logic_vector(1 downto 0);
signal s_axi_bvalid_reg       : std_logic;
signal s_axi_arready_reg      : std_logic;
signal s_axi_rdata_reg        : std_logic_vector(C_IPIF_DBUS_WIDTH-1 downto 0);
signal s_axi_rresp_reg        : std_logic_vector(1 downto 0);
signal s_axi_rvalid_reg       : std_logic;
signal ipif_addr              : std_logic_vector(C_IPIF_ABUS_WIDTH-1 downto 0);
signal axi_addr               : std_logic_vector(C_IPIF_ABUS_WIDTH-1 downto 0);
signal bus2ip_addr_reg        : std_logic_vector(C_IPIF_ABUS_WIDTH-1 downto 0);
signal axi_avalid             : std_logic;
signal axi_avalid_reg         : std_logic;
-- Intermediate IPIC signals
signal bus2ip_addr_i          : std_logic_vector
                                ((C_IPIF_DBUS_WIDTH-1) downto 0);
signal bus2ip_rnw_i           : std_logic;
signal bus2ip_be_i            : std_logic_vector
                                (((C_IPIF_DBUS_WIDTH/8)-1) downto 0);
signal bus2ip_rnw_reg         : std_logic;
signal bus2ip_be_reg          : std_logic_vector
                                (((C_IPIF_DBUS_WIDTH/8)-1) downto 0);
-- Combined decoder signals
signal bus2ip_cs_i            : std_logic_vector((CS_BUS_SIZE-1) downto 0);
signal bus2ip_rdce_i          : std_logic_vector((CE_BUS_SIZE-1) downto 0);
signal bus2ip_wrce_i          : std_logic_vector((CE_BUS_SIZE-1) downto 0);
signal data_timeout           : std_logic;
signal counter_en_i           : std_logic;
signal counter_en_reg         : std_logic;
signal cs_ce_ld_enable_i      : std_logic;
signal clear_cs_ce_i          : std_logic;
signal dp_count_load          : std_logic;

  type BUS_ACCESS_STATES is (
    IDLE,
    READING,
    READ_WAIT,
    WRITE_WAIT,
    WRITING,
    B_VALID,
    BRESP_WAIT
  );
signal access_ns : BUS_ACCESS_STATES;
signal access_cs : BUS_ACCESS_STATES;
signal cs_for_gaps: std_logic;
-------------------------------------------------------------------------------
-- begin the architecture logic
-------------------------------------------------------------------------------
begin

-------------------------------------------------------------------------------
-- Address registered
-------------------------------------------------------------------------------
Bus2IP_Clk     <= S_AXI_ACLK;
Bus2IP_Resetn  <= S_AXI_ARESETN;
Bus2IP_RNW     <= bus2ip_rnw_reg;
Bus2IP_BE      <= bus2ip_be_reg;
Bus2IP_Data    <= S_AXI_WDATA;
Bus2IP_Addr    <= bus2ip_addr_reg;
Bus2IP_CS      <= bus2ip_cs_i;
Bus2IP_RdCE    <= bus2ip_rdce_i;
Bus2IP_WrCE    <= bus2ip_wrce_i;
-- AXI output signals
S_AXI_AWREADY  <= s_axi_awready_reg;
S_AXI_WREADY   <= s_axi_wready_reg;
S_AXI_BVALID   <= s_axi_bvalid_reg;
S_AXI_BRESP    <= s_axi_bresp_reg;
S_AXI_ARREADY  <= s_axi_arready_reg;
S_AXI_RRESP    <= s_axi_rresp_reg;
S_AXI_RVALID   <= s_axi_rvalid_reg;
S_AXI_RDATA    <= s_axi_rdata_reg;

-- Misc.
axi_addr       <= S_AXI_ARADDR when (S_AXI_ARVALID = '1') else S_AXI_AWADDR;

-- Mask off unused high-order address bits
ipif_addr      <= ZEROS & axi_addr(C_ADDR_DECODE_BITS downto 0);

-------------------------------------------------------------------------------
-- Address Decoder Component Instance
--
-- This component decodes the specified base address pairs and outputs the
-- specified number of chip enables and the target bus size.
-------------------------------------------------------------------------------
I_DECODER : address_decoder
    generic map
    (
     C_BUS_AWIDTH          => C_NUM_DECODE_BITS,
     C_S_AXI_MIN_SIZE      => C_S_AXI_MIN_SIZE,
     C_ARD_ADDR_RANGE_ARRAY=> C_ARD_ADDR_RANGE_ARRAY,
     C_ARD_NUM_CE_ARRAY    => C_ARD_NUM_CE_ARRAY,
     C_FAMILY              => "nofamily"
    )
    port map
    (
     Bus_clk               =>  S_AXI_ACLK,
     Bus_rst               =>  S_AXI_ARESETN,
     Address_In_Erly       =>  bus2ip_addr_i(C_ADDR_DECODE_BITS downto 0),
     Address_Valid_Erly    =>  axi_avalid,
     Bus_RNW               =>  bus2ip_rnw_reg,
     Bus_RNW_Erly          =>  bus2ip_rnw_i,
     CS_CE_ld_enable       =>  cs_ce_ld_enable_i,
     Clear_CS_CE_Reg       =>  clear_cs_ce_i,
     RW_CE_ld_enable       =>  cs_ce_ld_enable_i,
     CS_for_gaps           =>  CS_for_gaps,
      -- Decode output signals
     CS_Out                =>  bus2ip_cs_i,
     RdCE_Out              =>  bus2ip_rdce_i,
     WrCE_Out              =>  bus2ip_wrce_i
      );
-------------------------------------------------------------------------------
-- AXI Transaction Controller
-------------------------------------------------------------------------------
Access_Control : process (access_cs, ipif_addr, data_timeout,
    S_AXI_ARVALID, S_AXI_AWVALID, S_AXI_WVALID,S_AXI_RREADY, S_AXI_BREADY,
    S_AXI_WSTRB,IP2Bus_Data, IP2Bus_RdAck, IP2Bus_WrAck, IP2Bus_Error,
    axi_avalid_reg, s_axi_bvalid_reg, s_axi_bresp_reg,s_axi_rdata_reg,
    s_axi_rresp_reg, s_axi_rvalid_reg, bus2ip_addr_reg, bus2ip_rnw_reg,
    bus2ip_be_reg, counter_en_reg,CS_for_gaps) is
    begin
      access_ns <= access_cs;
      s_axi_arready_i    <= '1';
      s_axi_awready_i    <= '0';
      s_axi_wready_i     <= '0';
      s_axi_bvalid_i     <= s_axi_bvalid_reg;
      s_axi_bresp_i      <= s_axi_bresp_reg;
      s_axi_rdata_i      <= s_axi_rdata_reg;
      s_axi_rresp_i      <= s_axi_rresp_reg;
      s_axi_rvalid_i     <= s_axi_rvalid_reg;
      bus2ip_addr_i      <= bus2ip_addr_reg;
      bus2ip_rnw_i       <= bus2ip_rnw_reg;
      bus2ip_be_i        <= bus2ip_be_reg;
      cs_ce_ld_enable_i  <= '0';
      clear_cs_ce_i      <= '0';
      dp_count_load      <= '0';
      axi_avalid         <= axi_avalid_reg;
      counter_en_i       <= counter_en_reg;
        case access_cs is
          when IDLE =>
            if (S_AXI_ARVALID = '1') then  -- Read precedence over write
              s_axi_arready_i  <= '0';  -- 1-cycle pulse
              axi_avalid       <= '1';  -- sticky
              bus2ip_rnw_i     <= '1';
              bus2ip_addr_i    <= ipif_addr;
              bus2ip_be_i      <= "1111";
              cs_ce_ld_enable_i<= '1';
              counter_en_i     <= '1';
              dp_count_load    <= '1';
              access_ns        <= READING;
            elsif (S_AXI_AWVALID = '1') then
              s_axi_arready_i  <= '0';
              s_axi_awready_i  <= '1';  -- 1-cycle pulse
              axi_avalid       <= '1';
              bus2ip_rnw_i     <= '0';
              bus2ip_addr_i    <= ipif_addr;
              counter_en_i     <= '0';
              dp_count_load    <= '0';
              access_ns        <= WRITE_WAIT;
            else
              s_axi_arready_i  <= '1';
              access_ns        <= IDLE;
            end if;

          when READING =>
            s_axi_arready_i    <= '0';
            axi_avalid         <= '0';
            if (CS_for_gaps = '1') then
              s_axi_rvalid_i   <= '1';  -- Sticky
              s_axi_rdata_i    <= (others => '0');
              s_axi_rresp_i    <= AXI_RESP_OK;
              clear_cs_ce_i    <= '1';
              counter_en_i     <= '0';
              access_ns        <= READ_WAIT;
            elsif (data_timeout = '1') then
              s_axi_rvalid_i   <= '1';  -- Sticky
              s_axi_rdata_i    <= (others => '0');
              s_axi_rresp_i    <= AXI_RESP_SLVERR;
              clear_cs_ce_i    <= '1';
              counter_en_i     <= '0';
              access_ns        <= READ_WAIT;
            elsif (IP2Bus_RdAck = '1') then
              s_axi_rvalid_i   <= '1';  -- Sticky
              s_axi_rdata_i    <= IP2Bus_Data;
              if (IP2Bus_Error = '1') then
                s_axi_rresp_i  <= AXI_RESP_SLVERR;
              else
                s_axi_rresp_i  <= AXI_RESP_OK;
              end if;
              clear_cs_ce_i    <= '1';
              counter_en_i     <= '0';
              access_ns        <= READ_WAIT;
            end if;

          when READ_WAIT =>
           -- s_axi_arready_i    <= '0';
            counter_en_i       <= '0';
            axi_avalid <= '0';
            if (S_AXI_RREADY = '1') then
              s_axi_rvalid_i   <= '0';
              s_axi_rdata_i    <= (others => '0');
              s_axi_arready_i  <= '1';
              access_ns        <= IDLE;
            end if;

          when WRITE_WAIT =>
            s_axi_arready_i    <= '0';
            if (S_AXI_WVALID = '1') then
              counter_en_i     <= '1';
              dp_count_load    <= '1';
              axi_avalid       <= '1';
              cs_ce_ld_enable_i<= '1';
              bus2ip_be_i      <= S_AXI_WSTRB;
              access_ns        <= WRITING;
            end if;

          when WRITING =>
            s_axi_arready_i    <= '0';
            bus2ip_be_i        <= S_AXI_WSTRB;
            axi_avalid         <= '0';
            if CS_for_gaps = '1' then
               s_axi_wready_i <= '1';
               s_axi_bresp_i<= AXI_RESP_OK;
              -- axi_avalid       <= '0';
               cs_ce_ld_enable_i<= '0';
               clear_cs_ce_i    <= '1';
               counter_en_i     <= '0';
              -- bus2ip_be_i      <= S_AXI_WSTRB;
               access_ns        <= B_VALID;
            elsif data_timeout = '1' then
               s_axi_wready_i <= '1';
               s_axi_bresp_i<= AXI_RESP_SLVERR;
              -- axi_avalid       <= '0';
               cs_ce_ld_enable_i<= '0';
               clear_cs_ce_i    <= '1';
               counter_en_i     <= '0';
              -- bus2ip_be_i      <= S_AXI_WSTRB;
               access_ns        <= B_VALID;
            elsif IP2Bus_WrAck = '1' then
              s_axi_wready_i <= '1';
              if (IP2Bus_Error = '1') then
                s_axi_bresp_i<= AXI_RESP_SLVERR;
              else
                s_axi_bresp_i<= AXI_RESP_OK;
              end if;
             -- axi_avalid       <= '0';
              cs_ce_ld_enable_i<= '0';
              clear_cs_ce_i    <= '1';
              counter_en_i     <= '0';
             -- bus2ip_be_i      <= S_AXI_WSTRB;
              access_ns        <= B_VALID;
            end if;

          when B_VALID =>
            s_axi_arready_i    <= '0';
            s_axi_bvalid_i     <= '1';
            access_ns          <= BRESP_WAIT;

          when BRESP_WAIT =>
           -- s_axi_arready_i    <= '0';
            counter_en_i       <= '0';
            axi_avalid         <= '0';
            if (S_AXI_BREADY = '1') then
              s_axi_arready_i  <= '1';
              s_axi_bvalid_i   <= '0';
              access_ns        <= IDLE;
            end if;
        end case;
  end process Access_Control;

-------------------------------------------------------------------------------
  -- AXI Transaction Controller signals registered
-------------------------------------------------------------------------------

  Access_Control_Reg : process (S_AXI_ACLK) is
  begin
    if S_AXI_ACLK'event and S_AXI_ACLK = '1' then
      if S_AXI_ARESETN = '0' then
        access_cs             <= IDLE;
        s_axi_awready_reg     <= '0';
        s_axi_wready_reg      <= '0';
        s_axi_bvalid_reg      <= '0';
        s_axi_bresp_reg       <= "00";
        s_axi_arready_reg     <= '0';
        s_axi_rdata_reg       <= (others => '0');
        s_axi_rresp_reg       <= "00";
        s_axi_rvalid_reg      <= '0';
        bus2ip_addr_reg       <= (others => '0');
        bus2ip_rnw_reg        <= '0';
        axi_avalid_reg        <= '0';
	counter_en_reg        <= '0';
      else
        access_cs             <= access_ns;
        s_axi_awready_reg     <= s_axi_awready_i;
        s_axi_wready_reg      <= s_axi_wready_i;
        s_axi_bvalid_reg      <= s_axi_bvalid_i;
        s_axi_bresp_reg       <= s_axi_bresp_i;
        s_axi_arready_reg     <= s_axi_arready_i;
        s_axi_rdata_reg       <= s_axi_rdata_i;
        s_axi_rresp_reg       <= s_axi_rresp_i;
        s_axi_rvalid_reg      <= s_axi_rvalid_i;
        bus2ip_addr_reg       <= bus2ip_addr_i;
        bus2ip_rnw_reg        <= bus2ip_rnw_i;
        axi_avalid_reg        <= axi_avalid;
	counter_en_reg        <= counter_en_i;
      end if;
    end if;
  end process Access_Control_Reg;

-------------------------------------------------------------------------------
-- BE will be sent to the IPIC if C_USE_WSTRB = 1.
-------------------------------------------------------------------------------

 GEN_USE_WSTRB : if (C_USE_WSTRB = 1) generate
   begin
     process (S_AXI_ACLK) is
        begin
          if S_AXI_ACLK'event and S_AXI_ACLK = '1' then
            if S_AXI_ARESETN = '0' then
              bus2ip_be_reg <= "1111";
            else
              bus2ip_be_reg <= bus2ip_be_i;
            end if;
          end if;
     end process;
 end generate GEN_USE_WSTRB;

-------------------------------------------------------------------------------
-- BE to the IPIC will be "1111" if C_USE_WSTRB = 0.
-------------------------------------------------------------------------------

 GEN_NO_WSTRB : if (C_USE_WSTRB = 0) generate
   begin
     bus2ip_be_reg <= "1111";
 end generate GEN_NO_WSTRB;

-------------------------------------------------------------------------------
-- This implements the dataphase watchdog timeout function. The counter is
-- allowed to count down when an active IPIF operation is ongoing. A data
-- acknowledge from the target address space forces the counter to reload.
-------------------------------------------------------------------------------

DATA_PHASE_WDT : if (C_INCLUDE_DPHASE_TIMER /= 0) generate


constant TIMEOUT_VALUE_TO_USE : integer := C_DPHASE_TIMEOUT;
constant COUNTER_WIDTH        : Integer := clog2(TIMEOUT_VALUE_TO_USE);
constant DPTO_LD_VALUE        : std_logic_vector(COUNTER_WIDTH-1 downto 0)
                              := std_logic_vector(to_unsigned
                                 (TIMEOUT_VALUE_TO_USE-1,COUNTER_WIDTH));
signal dpto_cntr_ld_en        : std_logic;
signal dpto_cnt_en            : std_logic;
signal timeout_i              : std_logic;

begin

dpto_cntr_ld_en <= dp_count_load;

dpto_cnt_en <= counter_en_i; -- always enabled, load suppresses counting

I_DPTO_COUNTER : counter_f
  generic map(
    C_NUM_BITS    =>  COUNTER_WIDTH,
    C_FAMILY      => "nofamily"
      )
  port map(
    Clk           =>  S_AXI_ACLK,
    Rst           =>  '0',
    Load_In       =>  DPTO_LD_VALUE,
    Count_Enable  =>  dpto_cnt_en,
    Count_Load    =>  dpto_cntr_ld_en,
    Count_Down    =>  '1',
    Count_Out     =>  open,
    Carry_Out     =>  timeout_i
    );
REG_TIMEOUT : process(S_AXI_ACLK)
    begin
        if(S_AXI_ACLK'EVENT and S_AXI_ACLK='1')then
            if(S_AXI_ARESETN='0')then
                data_timeout <= '0';
            else
                data_timeout <= timeout_i;
            end if;
        end if;
    end process REG_TIMEOUT;
end generate DATA_PHASE_WDT;

NO_DATA_PHASE_WDT : if (C_INCLUDE_DPHASE_TIMER = 0) generate
   begin
     data_timeout <= '0';
end generate NO_DATA_PHASE_WDT;

end rtl;

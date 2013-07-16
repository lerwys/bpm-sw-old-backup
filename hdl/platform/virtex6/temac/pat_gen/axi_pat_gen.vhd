--------------------------------------------------------------------------------
-- File       : axi_pat_gen.vhd
-- Author     : Xilinx Inc.
-- Project    : Xilinx LogiCORE Virtex-6 Embedded Tri-Mode Ethernet MAC
-- File       : axi_pat_gen.vhd
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
-- Description:  This is a very simple pattern generator which will generate packets
-- with the supplied dest_addr and src_addr and incrementing data.  The packet size
-- increments between the min and max size (which can be set to the same value if a
-- specific size is required
--
--------------------------------------------------------------------------------

library unisim;
use unisim.vcomponents.all;

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.numeric_std.all;

entity axi_pat_gen is
   generic (
      DEST_ADDR            : bit_vector(47 downto 0) := X"da0102030405";
      SRC_ADDR             : bit_vector(47 downto 0) := X"5a0102030405";
      MY_WORD              : std_logic_vector(7 downto 0) := X"18"; -- j.b.
      MAX_SIZE             : unsigned(11 downto 0) := X"1f4";
      MIN_SIZE             : unsigned(11 downto 0) := X"040"
   );
   port (
      axi_tclk             : in  std_logic;
      axi_tresetn          : in  std_logic;

      enable_pat_gen       : in  std_logic;

      tdata                : out std_logic_vector(7 downto 0);
      tvalid               : out std_logic;
      tlast                : out std_logic;
      tready               : in  std_logic
   );
end axi_pat_gen;

architecture rtl of axi_pat_gen is

   -- State machine
   type state_typ is    (IDLE,
                         HEADER,
                         SIZE,
                         SET_DATA);

  type my_array is array (0 to 41) of integer range 0 to 255;
  constant my_word_array : my_array := (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12,
                                        13, 14, 15, 16, 17, 18, 19, 20, 21, 22,
                                        23, 24, 25, 26, 27, 28, 29, 30, 31, 32,
                                        33, 34, 35, 36, 37, 38, 39, 40, 41, 42); 
  signal sig_my_word_array    : unsigned(7 downto 0);

   signal next_gen_state      : state_typ;
   signal gen_state           : state_typ;

   signal byte_count          : unsigned(11 downto 0);
   signal header_count        : unsigned(3 downto 0);
   signal pkt_size            : unsigned(11 downto 0);
   signal lut_data            : std_logic_vector(7 downto 0);
   signal tvalid_int          : std_logic;

   signal axi_treset          : std_logic;

   constant dummy             : bit_vector(51 downto 0) := (others => '0');

  signal my_count             : unsigned(7 downto 0);
  signal sig_MY_WORD          : std_logic_vector(7 downto 0);

begin

   axi_treset <= not axi_tresetn;

   -- need a packet counter - max size limited to 11 bits
   byte_count_p : process (axi_tclk)
   begin
      if axi_tclk'event and axi_tclk = '1' then
         if axi_treset = '1' then
            byte_count <= (others => '0');
         elsif gen_state = SET_DATA and byte_count /= X"000" and tready = '1' then
            byte_count <= byte_count - X"001";
         elsif gen_state = IDLE then
            byte_count <= pkt_size;
         end if;
      end if;
   end process byte_count_p;

   -- need a smaller count to manage the header insertion
   header_count_p : process (axi_tclk)
   begin
      if axi_tclk'event and axi_tclk = '1' then
         if axi_treset = '1' then
            header_count <= (others => '0');
         elsif gen_state /= IDLE and header_count /= X"F" and (tready = '1' or tvalid_int = '0') then
            header_count <= header_count + X"1";
         elsif gen_state = IDLE then
            header_count <= (others => '0');
         end if;
      end if;
   end process header_count_p;

   -- need a smaller count to manage the header insertion
   -- adjust parameter values by 18 to allow for header and crc
   -- so the pkt_size can be sued directly in the size field
   pkt_size_p : process (axi_tclk)
   begin
      if axi_tclk'event and axi_tclk = '1' then
         if axi_treset = '1' then
            pkt_size <= MIN_SIZE - X"012";
         elsif gen_state = SET_DATA and next_gen_state = IDLE then
            if pkt_size = MAX_SIZE - X"012" then
               pkt_size <= MIN_SIZE - X"012";
            else
               pkt_size <= pkt_size + "001";
            end if;
         end if;
      end if;
   end process pkt_size_p;

   -- store the parametised values in a lut (64 deep)
   -- this should mean the values could be adjusted in fpga_editor etc..
   LUT6_gen : for I in 0 to 7 generate
   begin
      LUT6_inst : LUT6
      generic map (
         INIT           => dummy &
                           SRC_ADDR(I) & SRC_ADDR(I+8) & SRC_ADDR(I+16) &
                           SRC_ADDR(I+24) & SRC_ADDR(I+32) & SRC_ADDR(I+40) &
                           DEST_ADDR(I) & DEST_ADDR(I+8) & DEST_ADDR(I+16) &
                           DEST_ADDR(I+24) & DEST_ADDR(I+32) & DEST_ADDR(I+40)

      )
      port map (
        O    => lut_data(I),
        I0   => header_count(0),
        I1   => header_count(1),
        I2   => header_count(2),
        I3   => header_count(3),
        I4   => '0',
        I5   => '0'
      );
   end generate;

   -- simple state machine to control the data
   -- on the transition from IDLE we reset the counters and increment the packet size
   next_s : process(gen_state, enable_pat_gen, header_count, tready, byte_count, tvalid_int)
   begin
      next_gen_state <= gen_state;
      case gen_state is
         when IDLE =>
            if enable_pat_gen = '1' and tvalid_int = '0' then
                  next_gen_state <= HEADER;
            end if;
         when HEADER =>
            if header_count = X"b" and tready = '1' then
               next_gen_state <= SIZE;
            end if;
         when SIZE =>
            if header_count = X"d" and tready = '1' then
               next_gen_state <= SET_DATA;
            end if;
         when SET_DATA =>
            if byte_count = X"001" and tready = '1' then  -- may need to be 1
               next_gen_state <= IDLE;
            end if;
      end case;
   end process;

   state_p : process (axi_tclk)
   begin
      if axi_tclk'event and axi_tclk = '1' then
         if axi_treset = '1' then
            gen_state <= IDLE;
         else
            gen_state <= next_gen_state;
         end if;
      end if;
   end process state_p;


   -- now generate the TVALID output
   valid_p : process (axi_tclk)
   begin
      if axi_tclk'event and axi_tclk = '1' then
         if axi_treset = '1' then
            tvalid_int <= '0';
         elsif gen_state /= IDLE then
            tvalid_int <= '1';
         elsif tready = '1' then
            tvalid_int <= '0';
         end if;
      end if;
   end process valid_p;

   -- now generate the TDATA output
   data_p : process (axi_tclk)
   begin
      if axi_tclk'event and axi_tclk = '1' then
         if gen_state = HEADER and (tready = '1' or tvalid_int = '0') then
            tdata <= lut_data;
         elsif gen_state = SIZE and tready = '1' then
            if header_count(0) = '1' then
               tdata <= std_logic_vector(pkt_size(7 downto 0));
            else
               tdata <= "00000" & std_logic_vector(pkt_size(10 downto 8));
            end if;
         elsif tready = '1' then
            tdata <= sig_MY_WORD;
         end if;
      end if;
   end process data_p;


  -- generate MY_WORD                                                    -- j.b.
  my_cnt_p : process (axi_tclk)
  begin
    if axi_tclk'event and axi_tclk = '1' then
      if axi_treset = '1' then
        my_count <= (others => '0');
      elsif gen_state = SET_DATA and tready = '1' then --my_count /= X"3C" and tready = '1' then
        my_count <= my_count + X"01";
      elsif gen_state = IDLE or my_count >= X"3C" then
        my_count <= (others => '0');
      end if;
    end if;
  end process my_cnt_p;
  
  my_word_p : process (axi_tclk)
  begin
    if axi_tclk'event and axi_tclk = '1' then
      if axi_treset = '1' then
        sig_MY_WORD <= (others => '0');
      --elsif my_count < X"12" and tready = '1' then
      --  sig_MY_WORD <= (others => '0');
      elsif my_count >= X"03" and tready = '1' then --X"12" and tready = '1' then
        --sig_my_word_array <= unsigned());
        sig_MY_WORD <= std_logic_vector(to_unsigned(my_word_array(to_integer(my_count - X"03")),8)); --MY_WORD; --std_logic_vector(my_count(7 downto 0));
      --else sig_MY_WORD <= (others => '0');
      end if;
    end if;
  end process my_word_p;
  ------------------------------------------------------------------------------
  
   -- now generate the TLAST output
   last_p : process (axi_tclk)
   begin
      if axi_tclk'event and axi_tclk = '1' then
         if axi_treset = '1' then
            tlast <= '0';
         elsif byte_count = "001"  and tready = '1' then
            tlast <= '1';
         elsif tready = '1' then
            tlast <= '0';
         end if;
      end if;
   end process last_p;

   tvalid <= tvalid_int;

end rtl;


-------------------------------------------------------------------------------
-- Copyright (c) 2013 Xilinx, Inc.
-- All Rights Reserved
-------------------------------------------------------------------------------
--   ____  ____
--  /   /\/   /
-- /___/  \  /    Vendor     : Xilinx
-- \   \   \/     Version    : 13.4
--  \   \         Application: XILINX CORE Generator
--  /   /         Filename   : chipscope_vio_1_port_256_bits.vhd
-- /___/   /\     Timestamp  : Mon Feb 25 10:03:45 BRT 2013
-- \   \  /  \
--  \___\/\___\
--
-- Design Name: VHDL Synthesis Wrapper
-------------------------------------------------------------------------------
-- This wrapper is used to integrate with Project Navigator and PlanAhead

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
ENTITY chipscope_vio_1_port_256_bits IS
  port (
    CONTROL: inout std_logic_vector(35 downto 0);
    CLK: in std_logic;
    SYNC_OUT: out std_logic_vector(255 downto 0));
END chipscope_vio_1_port_256_bits;

ARCHITECTURE chipscope_vio_1_port_256_bits_a OF chipscope_vio_1_port_256_bits IS
BEGIN

END chipscope_vio_1_port_256_bits_a;

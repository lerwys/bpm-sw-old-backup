-------------------------------------------------------------------------------
-- Copyright (c) 2013 Xilinx, Inc.
-- All Rights Reserved
-------------------------------------------------------------------------------
--   ____  ____
--  /   /\/   /
-- /___/  \  /    Vendor     : Xilinx
-- \   \   \/     Version    : 13.4
--  \   \         Application: XILINX CORE Generator
--  /   /         Filename   : chipscope_ila_1port_1trg256b.vhd
-- /___/   /\     Timestamp  : Thu May 23 21:24:56 Hora oficial do Brasil 2013
-- \   \  /  \
--  \___\/\___\
--
-- Design Name: VHDL Synthesis Wrapper
-------------------------------------------------------------------------------
-- This wrapper is used to integrate with Project Navigator and PlanAhead

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
ENTITY chipscope_ila_1port_1trg256b IS
  port (
    CONTROL: inout std_logic_vector(35 downto 0);
    CLK: in std_logic;
    TRIG0: in std_logic_vector(255 downto 0));
END chipscope_ila_1port_1trg256b;

ARCHITECTURE chipscope_ila_1port_1trg256b_a OF chipscope_ila_1port_1trg256b IS
BEGIN

END chipscope_ila_1port_1trg256b_a;

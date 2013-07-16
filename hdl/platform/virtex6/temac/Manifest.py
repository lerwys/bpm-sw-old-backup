files = [ "mac_v2_2.vhd",
          "mac_layer_v2_2_block.vhd",
          "mac_layer_v2_2.ngc",
          "mac_layer_v2_2_fifo_block.vhd" ];

modules = {
  "local" : ["physical",
              "fifo",
              "common",
              "pat_gen",
              "axi_ipif" ] };

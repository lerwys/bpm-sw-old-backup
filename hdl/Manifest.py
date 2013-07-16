#fetchto = "ip_cores"

modules = { "local": [
                "modules/custom_wishbone",
                "modules/custom_common",
                "modules/rffe_top",
                "modules/ethmac",
                "modules/fabric",
                "modules/udp-ip",
                "ip_cores/general-cores",
                "ip_cores/etherbone-core",
				"ip_cores/dsp-cores",
                "platform/virtex6/chipscope",
                "platform/virtex6/temac"]
#     "git" : [
#     ]
    };

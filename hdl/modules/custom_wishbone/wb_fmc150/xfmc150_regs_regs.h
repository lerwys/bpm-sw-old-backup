/*
  Register definitions for slave core: FMC ADC/DAC interface registers

  * File           : xfmc150_regs_regs.h
  * Author         : auto-generated by wbgen2 from xfmc150.wb
  * Created        : Mon Oct  8 11:24:05 2012
  * Standard       : ANSI C

    THIS FILE WAS GENERATED BY wbgen2 FROM SOURCE FILE xfmc150.wb
    DO NOT HAND-EDIT UNLESS IT'S ABSOLUTELY NECESSARY!

*/

#ifndef __WBGEN2_REGDEFS_XFMC150_WB
#define __WBGEN2_REGDEFS_XFMC150_WB

#include <inttypes.h>

#if defined( __GNUC__)
#define PACKED __attribute__ ((packed))
#else
#error "Unsupported compiler?"
#endif

#ifndef __WBGEN2_MACROS_DEFINED__
#define __WBGEN2_MACROS_DEFINED__
#define WBGEN2_GEN_MASK(offset, size) (((1<<(size))-1) << (offset))
#define WBGEN2_GEN_WRITE(value, offset, size) (((value) & ((1<<(size))-1)) << (offset))
#define WBGEN2_GEN_READ(reg, offset, size) (((reg) >> (offset)) & ((1<<(size))-1))
#define WBGEN2_SIGN_EXTEND(value, bits) (((value) & (1<<bits) ? ~((1<<(bits))-1): 0 ) | (value))
#endif


/* definitions for register: Input Flags for Pulsing Registers */

/* definitions for register: Input Flags for FMC150 */

/* definitions for field: SPI Read/Write flag in reg: Input Flags for FMC150 */
#define FMC150_FLGS_IN_SPI_RW                 WBGEN2_GEN_MASK(0, 1)

/* definitions for field: External Clock for ADC in reg: Input Flags for FMC150 */
#define FMC150_FLGS_IN_EXT_CLK                WBGEN2_GEN_MASK(1, 1)

/* definitions for register: Address for Chips on FMC150 */

/* definitions for register: Data In for Chips on FMC150 */

/* definitions for register: Chipselect for Chips on FMC150 */

/* definitions for field: Chipselect for cdce72010 in reg: Chipselect for Chips on FMC150 */
#define FMC150_CS_CDCE72010                   WBGEN2_GEN_MASK(0, 1)

/* definitions for field: Chipselect for ads62p49 in reg: Chipselect for Chips on FMC150 */
#define FMC150_CS_ADS62P49                    WBGEN2_GEN_MASK(1, 1)

/* definitions for field: Chipselect for dac3283 in reg: Chipselect for Chips on FMC150 */
#define FMC150_CS_DAC3283                     WBGEN2_GEN_MASK(2, 1)

/* definitions for field: Chipselect for amc7823 in reg: Chipselect for Chips on FMC150 */
#define FMC150_CS_AMC7823                     WBGEN2_GEN_MASK(3, 1)

/* definitions for register: ADC Delay */

/* definitions for field: ADC Strobe delay in reg: ADC Delay */
#define FMC150_ADC_DLY_STR_MASK               WBGEN2_GEN_MASK(0, 5)
#define FMC150_ADC_DLY_STR_SHIFT              0
#define FMC150_ADC_DLY_STR_W(value)           WBGEN2_GEN_WRITE(value, 0, 5)
#define FMC150_ADC_DLY_STR_R(reg)             WBGEN2_GEN_READ(reg, 0, 5)

/* definitions for field: ADC Channel A delay in reg: ADC Delay */
#define FMC150_ADC_DLY_CHA_MASK               WBGEN2_GEN_MASK(8, 5)
#define FMC150_ADC_DLY_CHA_SHIFT              8
#define FMC150_ADC_DLY_CHA_W(value)           WBGEN2_GEN_WRITE(value, 8, 5)
#define FMC150_ADC_DLY_CHA_R(reg)             WBGEN2_GEN_READ(reg, 8, 5)

/* definitions for field: ADC Strobe delay in reg: ADC Delay */
#define FMC150_ADC_DLY_CHB_MASK               WBGEN2_GEN_MASK(16, 5)
#define FMC150_ADC_DLY_CHB_SHIFT              16
#define FMC150_ADC_DLY_CHB_W(value)           WBGEN2_GEN_WRITE(value, 16, 5)
#define FMC150_ADC_DLY_CHB_R(reg)             WBGEN2_GEN_READ(reg, 16, 5)

/* definitions for register: Data Out From Chips on FMC150 */

/* definitions for register: Flags out from Chips on FMC150 */

/* definitions for field: SPI Busy in reg: Flags out from Chips on FMC150 */
#define FMC150_FLGS_OUT_SPI_BUSY              WBGEN2_GEN_MASK(0, 1)

/* definitions for field: FPGA ADC clock locked in reg: Flags out from Chips on FMC150 */
#define FMC150_FLGS_OUT_ADC_CLK_LOCKED        WBGEN2_GEN_MASK(1, 1)

PACKED struct FMC150_WB {
  /* [0x0]: REG Input Flags for Pulsing Registers */
  uint32_t FLGS_PULSE;
  /* [0x4]: REG Input Flags for FMC150 */
  uint32_t FLGS_IN;
  /* [0x8]: REG Address for Chips on FMC150 */
  uint32_t ADDR;
  /* [0xc]: REG Data In for Chips on FMC150 */
  uint32_t DATA_IN;
  /* [0x10]: REG Chipselect for Chips on FMC150 */
  uint32_t CS;
  /* [0x14]: REG ADC Delay */
  uint32_t ADC_DLY;
  /* [0x18]: REG Data Out From Chips on FMC150 */
  uint32_t DATA_OUT;
  /* [0x1c]: REG Flags out from Chips on FMC150 */
  uint32_t FLGS_OUT;
};

#endif

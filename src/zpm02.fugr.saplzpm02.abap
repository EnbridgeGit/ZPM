*******************************************************************
*   System-defined Include-files.                                 *
*******************************************************************
  INCLUDE LZPM02TOP.                         " Global Data
  INCLUDE LZPM02UXX.                         " Function Modules

*******************************************************************
*   User-defined Include-files (if necessary).                    *
*******************************************************************
* INCLUDE LZPM02F...                         " Subroutines
* INCLUDE LZPM02O...                         " PBO-Modules
* INCLUDE LZPM02I...                         " PAI-Modules
* INCLUDE LZPM02E...                         " Events
* INCLUDE LZPM02P...                         " Local class implement.

INCLUDE LZPM02F01.
  INCLUDE LZPM02F00                               . " subprograms
  INCLUDE LZPM02I00                               . " PAI modules
  INCLUDE LSVIMFXX                                . " subprograms
  INCLUDE LSVIMOXX                                . " PBO modules
  INCLUDE LSVIMIXX                                . " PAI modules

INCLUDE LZPM02F02.

INCLUDE LZPM02F03.
*{   INSERT         D30K924502                                        1
*
INCLUDE Z_PMEQUIP_GLOBAL.
*}   INSERT

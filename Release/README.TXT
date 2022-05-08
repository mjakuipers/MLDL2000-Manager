//---------------------------------------------------------------------------//
//    Copyright (c) 2022  Meindert Kuipers, Netherlands                      //
//    meindert@kuiprs.nl                 www.kuiprs.nl                       //
//                                                                           //
// This program is free software; you can redistribute it and/or             //
// modify it under the terms of the GNU General Public License               //
// as published by the Free Software Foundation; either version 2            //
// of the License, or (at your option) any later version.                    //
//                                                                           //
// This program is distributed in the hope that it will be useful,           //
// but WITHOUT ANY WARRANTY; without even the implied warranty of            //
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the             //
// GNU General Public License for more details.                              //
//                                                                           //
// You should have received a copy of the GNU General Public License         //
// along with this program; if not, write to the Free Software               //
// Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA//
//---------------------------------------------------------------------------//
//
//---------------------------------------------------------------------------//
//  README.TXT                                                               //
//  File with installation and revision instructions                         //
//  Ver  Date     Description                                                //
//  1.50 May 2008 Final release                                              //
//  1.51 Dec 2008 See description below                                      //
//  1.60 Jan 2010 Beta release, see below                                    //
//  1.61 Feb 2010 Beta release, see below                                    //
//  1.70 Nov 2010 Final Release                                              //
//  1.90 Mar 2020 Beta release (not released yet)                            //
//  1.91 May 2022 Update of 1.91 with some issues fixed			     //
//---------------------------------------------------------------------------//

Changes in V1.91 Final Release
------------------------------------------------
- Fixed Range Check Error in ROM Header listing of 8K Rom
- Fixed display of ROM name in multiple bank ROMS in SR listing
- Add HP41CL WCMD to disassembler
- Fixed issue in saving INI file
- Fixed default disassembler settings
- Fixed issue in uploading SR's

IMPORTANT NOTE:
The Disassembler and MODFile Handler in M2kM will not be maintained anymore. 
These are now maintained in the standalone ROMHandler application which does not
have any MLDL2000 or other hardware dependencies. Please use this for up-to-date 
disassembly and taking care of MOD files.

M2kM is written in Object Pascal and compiled with Lazarus, tested on Windows 10.

Changes in V1.70 Final Release
------------------------------------------------
- you will have much more SRAM memory available!
- This version of M2kM is NOT compatible with firmware 1.60 and earlier. New firmware
  with a version 1.70 or newer MUST be installed in the MLDL2000 device. This may
  be done with M2kM version 1.70.
  It is strongly recommended to make a full backup first with the previous soft-
  and firmware versions.
- Settings Registers in the MLDL2000 are now 12 bits instead of 10 bits. This
  increases the amount of SRAM memory available for storing ROM images.
- SRAM can be erased with a button on the FLASH tab
- M2kM is now written in Free Pascal using the Lazarus IDE
- It is possible to import (with open) a MOD file that is enclosed in a i41CX
  email sent from an iPhone/iPod/iPad device. The email must be saved as an .html
  or .htm file first
- Various improvements to the disassembler are made
- The firmware may contain an Owner ID string. This must be entered BEFORE
  programming the firmware itself


Instructions for migrating to the new version firmware and software.
--------------------------------------------------------------------

STEP 1
  Use your current M2kM (NOT V1.70) software and firmware create a backup of 
  your MLDL2000 contents. Make a written note of your settings registers as these
  cannot be transferred back with a restore with the new software.
  
STEP 2
  Install the new (V1.70) M2kM sofware, especially the "fpcmemdll.dll" in a seperate
  directory by unzipping it. Connect your MLDL2000 and start "MLDL2K.exe"
  
STEP 3
  Verify the communication between the MLDL and your PC by checking the status line
  and list the contects of the MLDL in the Settings Registers tab.
  At this point, put the leftmost dipswitch in the lower position.
  
STEP 4
  When there is communication, select the men choice Tools -> CPLD Upgrade. Click
  Open, and locate and open the firmware file "MLDL2000 FW V1.70.xsvf". Check your 
  current firmware version and decide if you really want to go ahead with this.
  Type your name in the OWNER ID STRING box. Any other random string may be written
  here, but be aware that it can be changed only by reprogramming the firmware.
  
STEP 5
  Power your HP41, but do not run it. Firmware updating will work only when the MLDL
  is in your powered HP41.
  Now click Program. When faults are reported, change the JTAG Speed in the Preferences
  dialog and try again. 
  
STEP 6
  When succesful, Select MLDL2000 -> Disconnect, and Connect again. The statusline 
  should show the new firmware version and your Owner ID String, and this confirms
  that the firmware update was succesful.
  
STEP 7
  List the contents of your MLDL2000 in the Settings Register tab. Now manually restore
  all the settings registers. Worst case you may have to do a restore of the backup made 
  earlier, be aware that is is very likely that SRAM block 0 is changed.
  In case of a restore, you must manually restore and upload the settings registers for
  ALL SR numbers!

STEP 8
  In case you suspect problems, please contact me to have it resolved


Changes in V1.61 Beta release
- improved disassembler:
  - support for SDK41 compatible disassembly
  - added option for clean listing (no addresses or hexcodes)
  - fixed some issues with Class 0 mnemonics
  - disassembler source is now in seperate unit
- support for .ASC files (textual hex listing) for testing only

Changes in V1.60 Beta release
- improved disassembler:
  - internal structure of disassembler has totally changed
  - much more listing customizations possible with Preferences
  - generation of Cross Reference Table of the labels
  - fixed some minor errors in the disassembler
  - SDK41 disassembly in not yet implemented
- M2kM tested on Windows 7 and Windows XP
- no other changes

Known issues in V1.60 Beta:
- no SDK41 disassembly
- HP41 mcode I/O is EXPERIMENTAL, please refer to user manual!
  mcode I/O will only work with firmware V1.51 !!
- handling MOD files under Windows Vista may be problematic in some cases
- MOD files can not be read-only
- manual is not updated yet
- the development tool Turbo Explorer for Delphi is no longer supported by Embarcadero

Changes in V1.51 with respect to V1.50 are:
- improved disassembler:
  - added SDK41 assembly mode, still EXPERIMENTAL!
  - recognizes error labels: ERROR, MSG. MSGA
  - improved local label and XROM search
- added support for I/O mcode functions (new form) this is EXPERIMENTAL!
- fixed saving/restoring of screen window positions
- fixed overwriting of SRAM when checking firmware version
- changed IO / FLASH / SRAM in settings register, see specifications
- MOD files may not be read-only. MDO is supported as alternative
- improved empty check for FLASH
- added more labels to SYSTEMLABELS.TXT mainly from ROM 0
- numerous smaller bugs are fixed

Known issues:
- SDK41 disassembly is experimental
- HP41 mcode I/O is EXPERIMENTAL, please refer to user manual!
  mcode I/O will only work with firmware V1.51 !!
- handling MOD files under Windows Vista may be problematic
- MOD files can not be read-only

The executable of the MLDL2000 Manager software (M2kM) consist of the following
files:

MLDL2K.exe        - the executable
MLDL2K.INI        - ini file with preferences for M2kM
fpcmemdll.dll     - free pascal shared memory manager
SYSTEMLABELS.TXT  - textfile with labels for the disassembler
XROM.TXT          - textfile with XROM numbers for the disassembler
README.TXT        - this file

M2kM is written in Object Pascal and compiled with Lazarus.

All files should be installed in the same directory. M2kM has no installer and
does not use the Windows registry. Uninstalling is done by deleting the files
mentioned above.

The manual for the MLDL2000 contains a description of the use of the functions
in M2kM and other important imformation.

For correct operation with the MLDL2000 the FTDI D2xx USB drivers for the FT2232
controller under Windows XP must be installed. The drivers can be installed in
the standard way. These drivers and installation instrcutions can be downloaded
from the FTDI website: www.ftdichip.com





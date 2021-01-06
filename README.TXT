//---------------------------------------------------------------------------//
//    Copyright (c) 2020  Meindert Kuipers, Netherlands                      //
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
//  Ver       Date     Description                                           //
//  1.70      Nov 2010 Final release                                         //
//  1.90      Jan 2021 Final and last release                                //
//						   						     //
//---------------------------------------------------------------------------//

The executable of the MLDL2000 Manager software (M2kM) consist of the following
files:

MLDL2K.exe	  		- the executable
fpcmemdll.dll		- external memory manager
SYSTEMLABELS.TXT		- textfile with labels for the disassembler
XROM.TXT			- textfile with XROM numbers for the disassembler
README.TXT			- this file

All files should be installed in the same directory. M2kM has no installer and
does not use the Windows registry. Uninstalling is done by deleting the files
mentioned above.

The manual for the MLDL2000 contains a description of the use of the functions
in M2kM and other important imformation. Note that this manual is somewhat out
of date.

For correct operation with the MLDL2000 the FTDI D2xx USB drivers for the FT2232
controller under Windows XP must be installed. The drivers can be installed in
the standard way. These drivers and installation instrcutions can be downloaded
from the FTDI website: www.ftdichip.com. The FTD2XX.DLL must be accesible for the
M2kM application. This driver must be installed also when the MLDL2000 is not connected

Most important changes from the previous release
------------------------------------------------
- various bugs fixed
- many improvements in the disassembler
- support for DM41X .ram files, these are split into individual .rom files, the first
  rom image is opened
- hex dump of ROM images now possible
- can recognize 41CL ROM images using 
- disassembler supports 41CL WCMD instruction, does NOT decode useage of upper bits (yet)
- possibility to list 41CL FLDB .rom files (IMDB file support coming)
- FAT listing supports 41CL CRC
- listing from MLDL2000 supports checksum and CRC (slow), use checkbox upper right

This software is provided as is. Please backup your data first.
This is the last maintained version of the MLDL2000 Manager software. Only functions related 
to communication with the MLDL2000 will be maintained. All functionality of the ROM handler 
will be transferred and maintained in a seperate application.

NO WARRANTY!





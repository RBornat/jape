# Building Linux Jape installation


Revised February 2023


## Dependencies


 An Oracle or Adoptium Java (13 or later) (but *DO NOT* use the standard Java installation, 
 because it generates a huge jre included with Jape)
        
 OCaml (4.09 or later).

## Licence

    Jape is free software; you can redistribute it and/or modify it
    under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    Jape is distributed in the hope that it will be useful, but
    WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with jape; if not, write to the Free Software Foundation,
    Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA (or
    look at http://www.gnu.org).


## What to do

1. Clone Jape from github.com RBornat/jape -- probably you will want to get a recent tagged version. 
If you take from the tip of the tree, you may not get a working version.

1. 
        cd jape/distrib/CommonBuildResources/Linux
        make wrap

1. Move the LinuxJape.tgz file you produced somewhere convenient; unwrap it 
(probably by double-clicking); read the README\_INSTALL\_JAPE file that it contains.

1. It's possible to build a flatpak version: if you want to do that you can read the 
README files about flatpakking Jape.

Richard Bornat
7th Feb 2023

Complaints to the issues part of the Jape github repository, please.

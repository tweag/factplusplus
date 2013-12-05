/* This file is part of the FaCT++ DL reasoner
Copyright (C) 2013 by Dmitry Tsarkov

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

#include <fstream>

#include "SaveLoadManager.h"
#include "tNamedEntry.h"

bool
SaveLoadManager :: existsContent ( void ) const
{
	// context is there if a file can be opened
	return !std::ifstream(filename.c_str()).fail();
}

void
SaveLoadManager :: clearContent ( void ) const
{
	remove(filename.c_str());
}

void
SaveLoadManager :: prepare ( bool input )
{
	// close all previously open streams
	delete ip;
	delete op;
	ip = NULL;
	op = NULL;

	// open a new one
	if ( input )
		ip = new std::ifstream(filename.c_str());
	else
		op = new std::ofstream(filename.c_str());
}

void
SaveLoadManager :: registerE ( const TNamedEntry* p )
{
	neMap.add(const_cast<TNamedEntry*>(p));
	if ( p->getEntity() != NULL )
		eMap.add(const_cast<TNamedEntity*>(p->getEntity()));
}

#if 0
bool
SaveLoadManager :: clearContent ( void ) const
{
#ifdef WINDOWS
    SHFILEOPSTRUCT file_op = {
        NULL,
        FO_DELETE,
        dirname.c_str(),	// FIXME!! fully qualified name expected
        "",
        FOF_NOCONFIRMATION |
        FOF_NOERRORUI |
        FOF_SILENT,
        false,
        0,
        "" };
    SHFileOperation(&file_op);
#endif
}
#endif

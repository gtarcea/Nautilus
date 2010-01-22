#!/usr/bin/env python

"""DAV Server.

    python davserver 
    Copyright (C) 1999 Christian Scholz (ruebe@aachen.heimat.de)

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Library General Public License for more details.

    You should have received a copy of the GNU Library General Public
    License along with this library; if not, write to the Free
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

This is an example for using the DAV package.
Just define an interface class and subclass the
davserver class like shown below

"""


__version__ = "0.5"


from DAV.davserver import DAVRequestHandler
from data import dataclass

import BaseHTTPServer


class mydavHandler(DAVRequestHandler):

    IFACE_CLASS=dataclass()

    def get_userinfo(self,user,pw):
    	""" authenticate user """
	return 1
	if user=="test" and pw=="test":
		return 1
	return None


def test(HandlerClass = mydavHandler,
         ServerClass = BaseHTTPServer.HTTPServer):
    BaseHTTPServer.test(HandlerClass, ServerClass)


if __name__ == '__main__':
    test()

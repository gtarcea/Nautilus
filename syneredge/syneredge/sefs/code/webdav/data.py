#!/opt/python/bin/python

"""

example interface class which interfaces a filesystem

"""

import sys
import urlparse
import os
import time
from string import joinfields, split, lower

from DAV.constants import COLLECTION, OBJECT
from DAV.errors import *
from DAV.iface import *

from DAV.davcmd import copyone, copytree, moveone, movetree, delone, deltree


class dataclass(dav_interface):
	
	""" model a filesystem for DAV

	This class models a regular filesystem for the DAV server

	The basic URL will be http://localhost/
	And the underlying filesystem will be /tmp

	Thus http://localhost/gfx/pix will lead
	to /tmp/gfx/pix

	"""


	DIR="/tmp/mypc"
	BASEURI="http://localhost"

	def uri2local(self,uri):
		""" map uri in baseuri and local part """
		uparts=urlparse.urlparse(uri)
		fileloc=uparts[2][1:]
		filename=os.path.join(self.DIR,fileloc)
		return filename

	def local2uri(self,filename):
		""" map local filename to URI """
		pnum=len(split(self.DIR,"/"))
		parts=split(filename,"/")[pnum:]
		sparts="/"+joinfields(parts,"/")
		uri=urlparse.urljoin(self.BASEURI,sparts)
		return uri


	def get_childs(self,uri):
		""" return the child objects as URIs for the given URI """
		fileloc=self.uri2local(uri)
		files=os.listdir(fileloc)
		filelist=[]
		for file in files:
			newloc=os.path.join(fileloc,file)
			filelist.append(self.local2uri(newloc))
		return filelist

	def get_data(self,uri):
		""" return the content of an object """
		path=self.uri2local(uri)
		if os.path.isfile(path):
			s=""
			fp=open(path,"r")
			while 1:
				a=fp.read()
				if not a: break
				s=s+a
			fp.close()
			return s
		else:
		    # also raise an error for collections
		    # don't know what should happen then..
		    raise DAV_NotFound


	def _get_dav_resourcetype(self,uri):
		""" return type of object """
		path=self.uri2local(uri)
		if os.path.isfile(path):
			return OBJECT
		else:
			return COLLECTION
		
	def _get_dav_displayname(self,uri):
	    raise DAV_Secret	# do not show

	def _get_dav_getcontentlength(self,uri):
		""" return the content length of an object """
		path=self.uri2local(uri)
		if os.path.isfile(path):
			s=os.stat(path)
			return str(s[6])
		else:
			return "0"

	def get_lastmodified(self,uri):
		""" return the last modified date of the object """
		path=self.uri2local(uri)
		s=os.stat(path)
		date=s[8]
		return date

	def get_creationdate(self,uri):
		""" return the last modified date of the object """
		path=self.uri2local(uri)
		s=os.stat(path)
		date=s[9]
		return date

	def _get_dav_getcontenttype(self,uri):
		""" find out yourself! """
		return "text/html"
		path=self.uri2local(uri)
		if os.path.isfile(path):
			return "application/octet-stream"
		else:
			return "httpd/unix-directory"


	def put(self,uri,data,content_type=None):
		""" put the object into the filesystem """
		path=self.uri2local(uri)
		try:
			fp=open(path,"w+")
			fp.write(data)
			fp.close()
		except:
			raise DAV_Error, 424
		return None

	def mkcol(self,uri):
		""" create a new collection """
		path=self.uri2local(uri)
		# remove leading slash
		if path[-1]=="/": path=path[:-1]

		# test if file already exists
		if os.path.exists(path):
			raise DAV_Error,405

		# test if parent exists
		h,t=os.path.split(path)
		if not os.path.exists(h):
			raise DAV_Error, 409

		# test, if we are allowed to create it
		try:
			os.system("mkdir '%s'" %path)
			return 201
		except:
			raise DAV_Forbidden

	### ?? should we do the handler stuff for DELETE, too ?
	### (see below)

	def rmcol(self,uri):
		""" delete a collection """
		path=self.uri2local(uri)
		if not os.path.exists(path):
		    raise DAV_NotFound
		if not os.system("rmdir '%s'" %path):
			return 204
		else:
			raise DAV_Forbidden	# forbidden

	def rm(self,uri):
		""" delete a normal resource """
		path=self.uri2local(uri)
		if not os.path.exists(path):
		    raise DAV_NotFound
		if not os.system("rm -f '%s'" %path):
			return 204
		else:
			raise DAV_Forbidden	# forbidden

	###
	### DELETE handlers (examples)
	### (we use the predefined methods in davcmd instead of doing
	### a rm directly
	###

	def delone(self,uri):
	    """ delete a single resource 

	    You have to return a result dict of the form
	    uri:error_code
	    or None if everything's ok
	    
	    """
	    return delone(self,uri)

	def deltree(self,uri):
	    """ delete a collection 

	    You have to return a result dict of the form
	    uri:error_code
	    or None if everything's ok
	    """

	    return deltree(self,uri)


	###
	### MOVE handlers (examples)
	###

	def moveone(self,src,dst,overwrite):
	    """ move one resource with Depth=0 

	    an alternative implementation would be

		result_code=201
		if overwrite: 
		    result_code=204
		    r=os.system("rm -f '%s'" %dst)
		    if r: return 412
		r=os.system("mv '%s' '%s'" %(src,dst))
		if r: return 412
		return result_code
	   
	    (untested!). This would not use the davcmd functions
	    and thus can only detect errors directly on the root node.
	    """
	    return moveone(self,src,dst,overwrite)

	def movetree(self,src,dst,overwrite):
	    """ move a collection with Depth=infinity

	    an alternative implementation would be

		result_code=201
		if overwrite: 
		    result_code=204
		    r=os.system("rm -rf '%s'" %dst)
		    if r: return 412
		r=os.system("mv '%s' '%s'" %(src,dst))
		if r: return 412
		return result_code
	   
	    (untested!). This would not use the davcmd functions
	    and thus can only detect errors directly on the root node"""

	    return movetree(self,src,dst,overwrite)

	###
	### COPY handlers
	###

	def copyone(self,src,dst,overwrite):
	    """ copy one resource with Depth=0 

	    an alternative implementation would be

		result_code=201
		if overwrite: 
		    result_code=204
		    r=os.system("rm -f '%s'" %dst)
		    if r: return 412
		r=os.system("cp '%s' '%s'" %(src,dst))
		if r: return 412
		return result_code
	   
	    (untested!). This would not use the davcmd functions
	    and thus can only detect errors directly on the root node.
	    """
	    return copyone(self,src,dst,overwrite)

	def copytree(self,src,dst,overwrite):
	    """ copy a collection with Depth=infinity

	    an alternative implementation would be

		result_code=201
		if overwrite: 
		    result_code=204
		    r=os.system("rm -rf '%s'" %dst)
		    if r: return 412
		r=os.system("cp -r '%s' '%s'" %(src,dst))
		if r: return 412
		return result_code
	   
	    (untested!). This would not use the davcmd functions
	    and thus can only detect errors directly on the root node"""

	    return copytree(self,src,dst,overwrite)

	###
	### copy methods.
	### This methods actually copy something. low-level
	### They are called by the davcmd utility functions
	### copytree and copyone (not the above!)
	### Look in davcmd.py for further details.
	###

	def copy(self,src,dst):
	    """ copy a resource from src to dst """
	    srcfile=self.uri2local(src)
	    dstfile=self.uri2local(dst)
	    try:
		os.system("cp '%s' '%s'" %(srcfile,dstfile))
	    except:
		raise DAV_Error, Forbidden

	def copycol(self,src,dst):
	    """ copy a collection.

	    As this is not recursive (the davserver recurses itself)
	    we will only create a new directory here. For some more
	    advanced systems we might also have to copy properties from
	    the source to the destination.
	    """

	    return self.mkcol(dst)


	def exists(self,uri):
	    """ test if a resource exists """
	    path=self.uri2local(uri)
	    if os.path.exists(path):
		return 1
	    return None

	def is_collection(self,uri):
		""" test if the given uri is a collection """
		path=self.uri2local(uri)
		if os.path.isdir(path):
			return 1
		else:
			return 0

if __name__=="__main__":
	c=dataclass()
	print c.get_childs("http://localhost/")

	from DAV.utils import create_treelist
	l=create_treelist(c,"http://localhost/")
	walk(tt,l)



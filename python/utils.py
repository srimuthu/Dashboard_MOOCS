#-------------------------------------------------------------------------------
# File Name : utils.py
# Purpose   : Utility classes for the dashboard backend
# Author    : S.M.N.Balasubramanian
# Created   : 24 Nov 2016
# Copyright : TBD
#-------------------------------------------------------------------------------

import sys, os
import xml.etree.ElementTree as et
import zipfile, fnmatch
import shutil
from error import *

xml_filename = 'config.xml'
compression_pattern = '*.zip'

class configParser():

	def __init__(self):
		"""
		Initialize the config file parser.
		The config file ideally has the environment info
		"""
		self.__initSuccess = False
		self.__courseSlugs = []
		self.__downloadPath = ''
		if os.path.isfile(xml_filename):
			self.__tree = et.ElementTree(file = xml_filename)
			self.__root = self.__tree.getroot()
			self.__initSuccess = True

	def parse_config_xml(self):
		"""
		Parse the xml file and return the relevant data
		"""

		if not self.__initSuccess:
			sys.exit(error.ConfigFileNotFoundError)

		for item in self.__root.iter():
			if item.tag == 'courses':
				for slug in item:
					if slug.tag == "slug":
						self.__courseSlugs.append(slug.text)
			if item.tag == 'datapath':
				self.__downloadPath = item.text
				if not os.path.exists(self.__downloadPath):
					sys.exit(error.NoDownloadPathError)

		return self.__courseSlugs, self.__downloadPath

class fileOps():

	def __init__(self):
		self.__initSuccess = True

	def delete_folder_contents(self, path):

		shutil.rmtree(path)
		os.makedirs(path)

	def backup_data(self, source, destination):
		pass

	def unzip_data(self, parentPath):

		for root, dirs, files in os.walk(parentPath):
			for filename in fnmatch.filter(files, compression_pattern):
				print(os.path.join(root, filename))
				zipfile.ZipFile(os.path.join(root, filename)).extractall(os.path.join(root, os.path.splitext(filename)[0]))

	def get_subdirs(self, path):

		return [name for name in os.listdir(path)
            if os.path.isdir(os.path.join(path, name))]


#Main function for testing purposes
if __name__=='__main__':
	# obj = configParser()
	# cs, dlp = obj.parse_config_xml()
	# print(cs,dlp)
	obj1 = fileOps()
	obj1.unzip_data('/home/smnbalasubramanian/MOOCS_Dev/tempdata/tables')
	#obj1.delete_folder_contents('/home/smnbalasubramanian/MOOCS_Dev/Dummy')
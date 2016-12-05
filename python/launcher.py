#-------------------------------------------------------------------------------
# File Name : launcher.py
# Purpose   : Launcher script for the dashboard backend
# Author    : S.M.N.Balasubramanian
# Created   : 26 Nov 2016
# Copyright : TBD
#-------------------------------------------------------------------------------


import os, subprocess
import convert_ondemand
import convert_ondemand_config
import utils
from error import *

if __name__=='__main__':
	"""
	Launcher python script - Fetch all course downloads and dump into postgresql DBs
	"""


	#Initiate Utils objects
	parserObj = utils.configParser()
	fileOpsObj = utils.fileOps()

	#Read configuration xml file
	courseSlugs, downloadPath = parserObj.parse_config_xml()

	#Empty the downloads folder
	fileOpsObj.delete_folder_contents(downloadPath)

	#Download data
	courses = ""
	for course in courseSlugs:
		courses = courses + course + ", "
	subprocess.call(['python', 'call.py', "tables", courses[:-2], downloadPath, '--verbose'])

	#Unzip the data
	fileOpsObj.unzip_data(downloadPath)

	#Convert the unzipped data and dump to postgresql databases
	for courseSlug in courseSlugs:
		path_to_files = downloadPath+"/tables/"+courseSlug

		for folder in fileOpsObj.get_subdirs(path_to_files):
			if courseSlug.replace("-","_") in folder:
				path_to_files = path_to_files + "/" + folder
				break

		postgres_database_name = courseSlug.replace("-",'')
		convert_ondemand.convert_ondemand_main(path_to_files, postgres_database_name)
		
	#Create course list json for use in shiny dashboard
	parserObj.create_course_list_json()


	#Delete Utils objects
	del(parserObj)
	del(fileOpsObj)






	
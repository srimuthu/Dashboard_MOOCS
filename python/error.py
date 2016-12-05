#-------------------------------------------------------------------------------
# File Name : error.py
# Purpose   : Contains all the error strings used in the dashboard backend 
# Author    : S.M.N.Balasubramanian
# Created   : 24 Nov 2016
# Copyright : TBD
#-------------------------------------------------------------------------------

class error():
	"""
	Contains all the error strings
	"""

	ConfigFileNotFoundError = "Config XML file not found!"
	NoDownloadPathError = "Download path does not exist!"
	configNotParsedError = "Call parse_config_xml before creating json"
	dashboardPathError = "Path specified in the config xml file for the dashboard location does not exist!!"


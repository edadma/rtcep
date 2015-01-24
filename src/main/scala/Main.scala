package ca.hyperreal.rtcep

import java.io._


object Main extends App
{
	def parse( s: String ) =
		try
		{
			TestParser.parse( s, EOF )._1
		}
		catch
		{
			case e: Exception => /*e.printStackTrace*/e.getMessage
		}
	
	println( parse(""" [1, 2|Rest] """) )
}
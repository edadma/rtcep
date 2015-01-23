package ca.hyperreal.rtcep

import java.io._


object Main extends App
{
	def parse( s: String ) =
		try
		{
			TestParser.parse( s, '.' )
		}
		catch
		{
			case e: Exception => /*e.printStackTrace*/e.getMessage
		}
	
	println( parse(""" [1|[2, 3|[4]]]. """) )
}
package ca.hyperreal.rtcep

import java.io._


object Main extends App
{
//	println( Lexer.chrStream(new StringReader("a\nb"), 4).mkString(",") )
	
// 	val l =
// 		new Lexer( 4 )
// 		{
// 			add(
// 				new AtomLexeme
// 				{
// 					add( "and" )
// 				} )
// 			add( NumberLexeme )
// 			ignore( new LineCommentLexeme("%") )
// 			ignore( new BlockCommentLexeme("/*", "*/") )
// 			add( StringLexeme )
// 			add(
// 				new SymbolLexeme
// 				{
// 					add( "+" )
// 					add( "*" )
// 					add( "**" )
// 					add( "(" )
// 					add( ")" )
// 				} )
// 			ignore( WhitespaceLexeme )
// 			add( EOFLexeme )
// 		}
// 		
// 	println( l.scan(new StringReader("""1 "as\\df" b""")).mkString(",") )
	
	val p = TestParser
		
	def parse( s: String ) =
		try
		{
			p.parse( s )
		}
		catch
		{
			case e: Exception => e.getMessage
		}
	
	println( parse(""" 1 + a( 1, 2 ) """) )
}
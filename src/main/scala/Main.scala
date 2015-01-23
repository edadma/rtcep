package ca.hyperreal.rtcep

import java.io._


object Main extends App
{
//	println( Lexer.chrStream(new StringReader("a\nb"), 4).mkString(",") )
	
// 	val l =
// 		new Lexer( 4 )
// 		{
// 			add(
// 				new AtomLexeme( 'atom )
// 				{
// 					add( "and" )
// 				} )
// 			add( new VariableLexeme('variable) )
// 			ignore( new LineCommentLexeme("%") )
// 			ignore( new BlockCommentLexeme("/*", "*/") )
// 			add( new IntegerLexeme('integer) )
// 			add( new StringLexeme('string, '`') )
// 			add(
// 				new SymbolLexeme( 'atom )
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
// 	println( l.scan(new StringReader("""Var""")).mkString(",") )
	
	val p = TestParser
		
	def parse( s: String ) =
		try
		{
			p.parse( s )
		}
		catch
		{
			case e: Exception => /*e.printStackTrace*/e.getMessage
		}
	
	println( parse(""" [1|[2, 3|[4]]] """) )
}
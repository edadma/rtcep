package ca.hyperreal.rtcep

import java.io._

import util.matching.Regex
import collection.mutable.{HashMap, ArrayBuffer}


class Lexer( tab: Int )
{
	private val lexemes = new ArrayBuffer[Lexeme]
	private val ignored = new ArrayBuffer[Lexeme]
	
	def add( l: Lexeme )
	{
		lexemes += l
	}
	
	def ignore( l: Lexeme )
	{
		add( l )
		ignored += l
	}
	
	def scan( r: Reader ): Stream[Token] =
	{
	var s = Lexer.chrStream( r, tab )
	
		def next: Token =
			if (s isEmpty)
				sys.error( "scan.next - empty character stream" )
			else
			{
				def search: (Lexeme, Token) =
				{
					for (l <- lexemes)
					{
						l.token(s) match
						{
							case None =>
							case Some( (rest, tok) ) =>
								s = rest
								return (l, tok)
						}
					}
				
					sys.error( "unrecognized character: " + s.head )
				}
				
			val (l, tok) = search
			
				if (ignored contains l)
					next
				else
					tok
			}

		def loop: Stream[Token] =
			next #:: (if (s isEmpty) Stream.empty else loop)
		
		loop
	}
	
}

abstract class Lexeme
{
	protected val JUNK = Token( null, null, null )
	
	private val EMPTYMAP = Map.empty[String, Any]
	
	def skip( s: Stream[Chr], cond: Char => Boolean ): Stream[Chr] =
		if (cond( s.head.ch ))
			skip( s.tail, cond )
		else
			s
	
	def skip( s: Stream[Chr], chars: Int ): Stream[Chr] =
		if (chars > 0)
			skip( s.tail, chars - 1 )
		else
			s
			
	def consume( s: Stream[Chr], str: String ) =
	{
	val it = str.iterator
	
		def _consume( _s: Stream[Chr] ): Option[Stream[Chr]] =
			if (it.hasNext)
				if (_s.head.ch == it.next)
					_consume( _s.tail )
				else
					None
			else
				Some( _s )
				
		_consume( s )
	}
	
	def consume( kind: Any, s: Stream[Chr], cond: Char => Boolean,
				prefix: String = "",
				error: String = "invalid token",
				matcher: Regex = null,
				mapping: collection.Map[String, Any] = EMPTYMAP,
				notafter: Char => Boolean = _ => false
				): (Stream[Chr], Token) =
	{
	val buf = new StringBuilder
		
		def _consume( c: Stream[Chr] ): Stream[Chr] =
			if (cond( c.head.ch ))
			{
				buf += c.head.ch
				_consume( c.tail )
			}
			else
				c
		
	val s1 = _consume( s )
	val v = buf.toString
	val (v1, s2) =
		if (matcher eq null)
			(v, s1)
		else
			matcher.findPrefixMatchOf( v ) match
			{
				case None => s.head.pos.error( error )
				case Some( m ) =>
					m.groupCount match
					{
						case 0 => (m.group( 0 ), skip( s, m.group(0).length ))
						case 1 => (m.group( 1 ), skip( s, m.group(1).length ))
						case g => sys.error( s"matcher '$matcher' had $g capturing groups" )
					}
			}
			
		if (notafter( s2.head.ch ))
			s.head.pos.error( error )
			
		mapping.get(v1) match
		{
			case None => (s2, Token( kind, prefix + v1, s.head.pos ))
			case Some( sym ) => (s2, Token( sym, prefix + v1, s.head.pos ))
		}
	}
	
	def token( s: Stream[Chr] ): Option[(Stream[Chr], Token)]
}

object EOFLexeme extends Lexeme
{
	def token( s: Stream[Chr] ) =
		if (s.head.ch == EOF)
			Some( (s.tail, Token(EOF, "<eof>", s.head.pos)) )
		else
			None
}

class SymbolLexeme extends Lexeme
{
	private val nonsymbol = (('\u0000' to ' ') ++ ('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9')).toSet
	private val symbols = new HashMap[String, Any]
	private var regex: Regex = null
	
	def reserved( s: String ) = symbols contains s
	
	def add( syms: String* )
	{
		for (s <- syms)
		{
			if (s == EOF.toString)
				sys.error( "EOT (end-of-transmission) character is a reserved symbol" )
				
			if (symbols contains s)
				sys.error( "symbol has already been added: " + s )
				
			symbols(s) =
				if (s.length == 1)
					s.head
				else
					Symbol(s)
		}
		
		regex = (symbols.keys.toSeq.sortWith( _ > _ ).map( Regex.quote ).mkString( "|" ) + "|.*").r
	}
	
	def token( s: Stream[Chr] ) =
		if (nonsymbol(s.head.ch))
			None
		else
			Some( consume('atom, s, !nonsymbol(_), matcher = regex, mapping = symbols) )
}

class AtomLexeme extends Lexeme
{
	private val lowercase = 'a' to 'z'
	private val keywords = new HashMap[String, Symbol]
	
	def reserved( s: String ) = keywords contains s
	
	def add( keys: String* )
	{
		for (k <- keys)
		{
			if (keywords contains k)
				sys.error( "keyword has already been added: " + k )
				
			keywords(k) = Symbol(k)
		}
	}
	
	def token( s: Stream[Chr] ) =
		if (lowercase contains s.head.ch)
			Some( consume('atom, s, ch => ch.isLetter || ch.isDigit || ch == '_', mapping = keywords) )
		else
			None
}

object VariableLexeme extends Lexeme
{
	private val variableStart = ('A' to 'Z').toSet + '_'
	private val variableRest = variableStart ++ ('0' to '9')
	
	def token( s: Stream[Chr] ) =
		if (variableStart(s.head.ch))
			Some( consume('variable, s, variableRest(_)) )
		else
			None
}

object NumberLexeme extends Lexeme
{
	def token( s: Stream[Chr] ) =
		if (!s.head.ch.isDigit)
			None
		else
			Some( consume('number, s, _.isDigit, error = "invalid literal number", notafter = c => c.isLetter || c == '_') )
}

object WhitespaceLexeme extends Lexeme
{
	def token( s: Stream[Chr] ) =
		if (s.head.ch.isWhitespace)
			Some( (skip(s, _.isWhitespace), JUNK) )
		else
			None
}

class LineCommentLexeme( start: String ) extends Lexeme
{
	def token( s: Stream[Chr] ) =
		consume( s, start ) match
		{
			case None => None
			case Some( s1 ) => Some( (skip(s, _ != '\n'), JUNK) )
		}
}

class BlockCommentLexeme( start: String, end: String, error: String ) extends Lexeme
{
	def token( s: Stream[Chr] ) =
		consume( s, start ) match
		{
			case None => None
			case Some( s1 ) =>
				def wade( _s: Stream[Chr] ): Stream[Chr] =
				{
					if (_s.head.end)
						_s.head.pos.error( error )
					else
						consume( _s, end ) match
						{
							case None => wade( _s.tail )
							case Some( _s1 ) => _s1
						}
				}
				
				Some( (wade(s), JUNK) )
		}
}

object Lexer
{
	def chrStream( r: Reader, tab: Int ): Stream[Chr] =
	{
	val br = if (r.isInstanceOf[BufferedReader]) r.asInstanceOf[BufferedReader] else new BufferedReader( r )
	var s = ""
	var s1 = br.readLine
	var line = 0
	var col = 0
	
		def next: Chr =
		{
			if (col == s.length)
			{
			val olds = s
			
				s = s1
				
				if (s eq null)
					return new Chr( EOF, new Position(if (line == 0) 1 else line, col + 1, olds) )
				else
				{
					s = s1
					s1 = br.readLine
					s = tabs2spaces( s, tab )
					
					if (s1 ne null)
						s += '\n'
						
					line += 1
					
					if (s == "")
					{
						s = null
						return new Chr( EOF, new Position(if (line == 0) 1 else line, 1, "") )
					}
					
					col = 0
				}
			}
			
		val c = s.charAt( col )

			col += 1
			
		val pos = new Position(line, col, s)
		
// 			if (c == EOF)
// 				pos.error( "EOT (end-of-transmission) character unexpectedly encountered in input stream" )
				
			new Chr( c, pos )
		}
		
		def loop: Stream[Chr] =
			next #:: (if (s eq null) Stream.empty else loop)
		
		loop
	}

	private def tabs2spaces( s: String, size: Int ) =
	{
		require( size > 0 )
		
	var line = s
	var index = 0
		
		while ({index = line.indexOf( '\t', index ); index > -1})
		{
		val pad = size - (index % size)
		
			line = line.substring( 0, index ) + " "*pad + line.substring( index + 1 )
			index += pad - 1
		}
		
		line
	}
}

class Position( val line: Int, val col: Int, val s: String )
{
	def inLine = s + "\n" + " "*(col - 1) + "^"
	
	def error( msg: String ) = sys.error( msg + " " + this + "\n" + inLine )
	
	override def toString = s"($line, $col)"
}

class Chr( val ch: Char, val pos: Position )
{
	def end = ch == EOF
	
	override def toString = if (end) s"<end $pos>" else "<'" + (if (ch == '\n') "\\n" else ch) + s"' $pos>"
}

case class Token( kind: Any, s: String, pos: Position )
{
	def end = kind == EOF
	
	override def toString = if (end) s"<end $pos>" else s"<[$kind] '" + s.replace( "\n", "\\n" )  + s"' $pos>"
}
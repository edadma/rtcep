package xyz.hyperreal.rtcep

import java.io._

import util.matching.Regex
import collection.mutable.{HashMap, ArrayBuffer}


class Lexer
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
	
	def scan( r: Reader, tab: Int ): Stream[Token] = scan( Lexer.stream(r, tab) )
	
	def scan( chr: Stream[Chr] ): Stream[Token] =
	{
	var s = chr
	
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
	protected val JUNK = Token( null, null, null, null )
	
	protected def skip( s: Stream[Chr], chars: Int ): Stream[Chr] =
		if (chars > 0)
			skip( s.tail, chars - 1 )
		else
			s
	
	protected def consume( s: Stream[Chr], chars: Int ): Option[(String, Stream[Chr])] =
	{
	val buf = new StringBuilder
	
		def _consume( _s: Stream[Chr], chars: Int ): Option[(String, Stream[Chr])] =
		{
			if (chars > 0)
				_s.head.ch match
				{
					case EOF => None
					case c =>
						buf += c
						_consume( _s.tail, chars - 1 )
				}
			else
				Some( (buf.toString, _s) )
		}
		
		_consume( s, chars )
	}
	
	protected def consume( s: Stream[Chr], str: String ) =
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
	
	protected def consume( kind: Any, s: Stream[Chr], cond: Char => Boolean,
				error: String = "invalid token",
				matcher: Regex = null,
				mapping: collection.Map[String, Any] = Map.empty,
				notafter: Char => Boolean = _ => false
				): (Stream[Chr], Token) =
	{
	val (v, s1) = consume( s, cond )
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
						case g => sys.error( s"matcher '$matcher' has $g capturing groups" )
					}
			}
			
		if (notafter( s2.head.ch ))
			s.head.pos.error( error )
			
		mapping.get(v1) match
		{
			case None => (s2, Token( kind, v1, s, s2 ))
			case Some( sym ) => (s2, Token( sym, v1, s, s2 ))
		}
	}

	private val ESCAPEMAP =
		Map(
			't' -> '\t',
			'r' -> '\r',
			'n' -> '\n',
			'f' -> '\f',
			'b' -> '\b',
			'"' -> '"',
			'\'' -> '\'',
			'`' -> '`',
			'\\' -> '\\'
		)

	protected def consume( tok: Any, s: Stream[Chr], delim: Char ): (Stream[Chr], Token) =
	{
	val buf = new StringBuilder
	
		def _token( _s: Stream[Chr] ): (Stream[Chr], Token) =
		{
			def enderror = _s.head.pos.error( "unexpected end of input in escape sequence" )
			
			_s.head.ch match
			{
				case EOF|'\n' => _s.head.pos.error( "unclosed string literal" )
				case '\\' =>
					_s.tail.head.ch match
					{
						case EOF => enderror
						case c if ESCAPEMAP contains c =>
							buf += ESCAPEMAP(c)
							_token( _s.tail.tail )
						case 'u' =>
							consume( _s.tail.tail, 4 ) match
							{
								case None => enderror
								case Some( (u, rest) ) =>
									buf +=
										(try
										{
											Integer.valueOf( u, 16 ).toChar
										}
										catch
										{
											case e: Exception => _s.tail.tail.head.pos.error( "invalid unicode value" )
										})
									_token( rest )
							}
						case _ => _s.tail.head.pos.error( "unrecognized escape character" )
					}
				case `delim` => (_s.tail, Token( tok, buf.toString, s, _s.tail ))
				case c =>
					buf += c
					_token( _s.tail )
			}
		}
		
		_token( s )
	}
	
	protected def consume( s: Stream[Chr], allow: Char => Boolean ): (String, Stream[Chr]) =
	{
	val buf = new StringBuilder
	
		def _consume( _s: Stream[Chr] ): (String, Stream[Chr]) =
			if (allow( _s.head.ch ))
			{
				buf += _s.head.ch
				_consume( _s.tail )
			}
			else
				(buf.toString, _s)
				
		_consume( s )
	}

	def token( s: Stream[Chr] ): Option[(Stream[Chr], Token)]
}

object EOFLexeme extends Lexeme
{
	def token( s: Stream[Chr] ) =
		if (s.head.ch == EOF)
			Some( (s.tail, Token(EOF, "<eof>", s, s.tail)) )
		else
			None
}

trait KeywordLexeme
{
	def reserved( s: String ): Boolean
	
	def add( keys: String* )
}

class SymbolLexeme( tok: Any, otherNonsymbols: Seq[Char] = List('_') ) extends Lexeme with KeywordLexeme
{
	private val nonsymbol = (('\u0000' to ' ') ++ ('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9')).toSet ++ otherNonsymbols
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
	
	def exempt( sym: String )
	{
		symbols(sym) = tok
	}
	
	def token( s: Stream[Chr] ) =
		if (nonsymbol(s.head.ch))
			None
		else
			Some( consume(tok, s, !nonsymbol(_), matcher = regex, mapping = symbols) )
}

class AtomLexeme( tok: Any ) extends Lexeme with KeywordLexeme
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
			Some( consume(tok, s, ch => ch.isLetter || ch.isDigit || ch == '_', mapping = keywords) )
		else
			None
}

class NameLexeme( tok: Any ) extends Lexeme
{
	private val identifier = (('a' to 'z') ++ ('A' to 'Z')).toSet
	
	def token( s: Stream[Chr] ) =
		if (identifier(s.head.ch))
			Some( consume(tok, s, identifier(_)) )
		else
			None
}

class VariableLexeme( tok: Any ) extends Lexeme
{
	private val variableStart = ('A' to 'Z').toSet + '_'
	private val variableRest = variableStart ++ ('a' to 'z') ++ ('0' to '9')
	
	def token( s: Stream[Chr] ) =
		if (variableStart(s.head.ch))
			Some( consume(tok, s, variableRest(_)) )
		else
			None
}

class StringLexeme( tok: Any, delim: Char ) extends Lexeme
{
	def token( s: Stream[Chr] ) =
		if (s.head.ch == delim)
			Some( consume(tok, s.tail, delim) )
		else
			None
}

class IntegerLexeme( tok: Any, notafterPred: Char => Boolean = (('a' to 'z') ++ ('A' to 'Z')).toSet + '_' ) extends Lexeme
{
	def token( s: Stream[Chr] ) =
		if (s.head.ch.isDigit)
			Some( consume(tok, s, _.isDigit, error = "invalid integer literal", notafter = notafterPred) )
		else
			None
}

class FloatingLexeme( tok: Any ) extends Lexeme
{
	private val FLOATCHARS = ('0' to '9').toSet + 'e' + 'E' + '.' + '+' + '-'
	private val FLOATMATCHER = """\d+?\.\d+(?:(?:E|e)(?:\+|\-)?\d+)?|\d+(?:E|e)(?:\+|\-)?\d+"""r
	
	def token( s: Stream[Chr] ) =
	{
	val (str, _) = consume( s, FLOATCHARS(_) )
	
		if (str == "")
			None
		else
		{
			FLOATMATCHER.findPrefixMatchOf( str ) match
			{
				case None => None
				case Some( m ) =>
					val d = m.group( 0 )
					val rest = skip( s, d.length )
					
					Some( (rest, Token( tok, d, s, rest )) )
			}
		}
	}
}

object WhitespaceLexeme extends Lexeme
{
	def token( s: Stream[Chr] ) =
		if (s.head.ch.isWhitespace)
			Some( (Lexer.skip(s.tail, _.isWhitespace), JUNK) )
		else
			None
}

class LineCommentLexeme( start: String ) extends Lexeme
{
	def token( s: Stream[Chr] ) =
		consume( s, start ) match
		{
			case None => None
			case Some( s1 ) => Some( (Lexer.skipLine(s), JUNK) )
		}
}

class BlockCommentLexeme( start: String, end: String ) extends Lexeme
{
	def token( s: Stream[Chr] ) =
		consume( s, start ) match
		{
			case None => None
			case Some( s1 ) =>
				def wade( _s: Stream[Chr] ): Stream[Chr] =
				{
					if (_s.head.end)
						_s.head.pos.error( "unclosed comment" )
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
	def stream( r: Reader, tab: Int ): Stream[Chr] =
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
	
	def skip( s: Stream[Chr], cond: Char => Boolean ): Stream[Chr] =
		if (!s.head.end && cond( s.head.ch ))
			skip( s.tail, cond )
		else
			s

	def skipLine( s: Stream[Chr] ) =
		Lexer.skip( s, _ != '\n' ) match
		{
			case s if s.head.end => s
			case s => s.tail
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
	def inLine = s + (if (s endsWith("\n")) "" else "\n") + " "*(col - 1) + "^"
	
	def error( msg: String ) = sys.error( msg + " " + this + "\n" + inLine )
	
	override def toString = s"($line, $col)"
}

class Chr( val ch: Char, val pos: Position )
{
	def end = ch == EOF
	
	override def toString = if (end) s"<end $pos>" else "<'" + (if (ch == '\n') "\\n" else ch) + s"' $pos>"
}

case class Token( kind: Any, s: String, start: Stream[Chr], rest: Stream[Chr] )
{
	def end = kind == EOF
	
	def pos = start.head.pos
	
	override def toString = if (end) s"<end $pos>" else s"<[$kind] '" + s.replace( "\n", "\\n" )  + s"' $pos>"
}
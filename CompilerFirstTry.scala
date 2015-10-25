import scala.io.Source
import scala.util.control._
import scala.collection.mutable.ListBuffer
/*val INTEGER = "INTEGER"
val PLUS = "PLUS"
val MINUS = "MINUS"
val EOF = "EOF"*/

//para multilineas, tomamos el token ; y a lo que sigue le aplicamos la gramatica


object CompilerFirstTry{ 

def main(args: Array[String]){ 
	
  	val in = new Interpreter(Source.fromFile(args(0)).mkString)

  	in.print()

 }
}


class Token(tipo : String, valor: Char) {
	def print(){
	println(tipo + " " +valor)}
	def getType=tipo
	
	def getVal=valor

	override def toString = "sad"
	
}

class Interpreter(archivo: String){
	def print(){
		println(archivo)
		val texto = archivo.split('\n').map(_.trim.filter(_ >= ' ')).mkString
		println(texto)
		var lex = new Lexer(texto)
		
	}

 }

 class Lexer(texto: String)
 {
 	val variables = List('a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z')
 	var pos = 0
 	var lista = texto.toList
 	var tokenList = new ListBuffer[Token]()
 	while(pos< texto.length()){
 		get_next_t(lista(pos))

		pos=pos+1
		
	}
	for(d<-tokenList)
	{
		d.print()
	}

	var pars = new Parser(tokenList)

	def get_next_t(c: Char)  
	{	
		println(pos)
		println(c)
		if(c.isDigit)
		{
			println("digit")
			tokenList += new Token("DIGITO", c)

		}
		if(c=='.')
		{
			tokenList += new Token("PUNTO", c)
		}
		if(c== ';')
		{
			tokenList += new Token("ENDL" , c)
		}
		if(c == '+')
		{
			println("suma")

			tokenList += new Token("SUMA", c)
			
		}
		if(c == '-')
		{
			println("resta")
			tokenList += new Token("RESTA", c)
		}
		
		if(c == '*')
		{
			println("mult")
			tokenList += new Token("MULTIPLICACION", c)
		}
		
		if(c == '/')
		{
			println("div")
			tokenList += new Token("DIVISION", c)
		}
		if(c == '^')
		{
			println("potencia")
			tokenList += new Token("POTENCIA",c)
		}

		if(c == '%')
		{
			println("modulo")
			tokenList += new Token("MODULO", c)
		}
		if(c == '=')
		{
			println("Igualdad")
			tokenList += new Token("IGUALDAD", c)
		}

		for(d<-variables)
		{
			if(c==d)
			{
				println("letra")
				tokenList += new Token("LETRA",c)
			}
		}
		
		

	}
 }

 class Parser(tokens: ListBuffer[Token])
 {
	val variables = List('a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z')
	var pos = 0;
	A()
//	tokens(pos)
	def A()
	{
		if(Y && tokens(pos).getType == "IGUALDAD")
		{println("=")}
		else if(true)
		{println("expresion")}
	}

	def E()
	{

	}

	def Y =
	{	
		var x = false
		if(tokens(pos).getType == "LETRA")
		{
			x = true
			if(tokens(pos).getVal == 'v' && tokens(pos+1).getVal == 'a' && tokens(pos+2).getVal == 'r' && tokens(pos+3).getType == "LETRA")
			{
				pos = pos + 3
				println("var" + tokens(pos).getVal )
				pos = pos + 1

			}
					
		}
		x	
	}
 }

 /*
 A -> Y = E | E
E -> X + E | X - E | X
X -> X * Z | X / Z | X % Z | Z
Z -> Z ^ F | F
F -> N | ( E ) | Y
Y -> a | b | c
N -> N 0 | N 1 | N 2 | N 3 | N 4 | N 5 | N 6 | N 7 | N 8 | N 9 | O | M .
O -> 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
M -> M 0 | M 1 | M 2 | M 3 | M 4 | M 5 | M 6 | M 7 | M 8 | M 9 | O*/
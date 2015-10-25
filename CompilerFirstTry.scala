import scala.io.Source
import scala.util.control._
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map
/*val INTEGER = "INTEGER"
val PLUS = "PLUS"
val MINUS = "MINUS"
val EOF = "EOF"*/

//para multilineas, tomamos el token ; y a lo que sigue le aplicamos la gramatica


object CompilerFirstTry{ 
	var variablef:Map[Char,Double] = Map()
	variablef += ('e' -> 2.718281828459045)
	variablef +=('p'->3.141592653589793)

	
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
		//println(archivo)
		val texto = archivo.split('\n').map(_.trim.filter(_ >= ' ')).mkString
		//println(texto)
		var lineas = texto.split(";")
		var lex = new Lexer(lineas)
	}

 }

 class Lexer(texto: Array[String])
 {


 	val variables = List('a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z')
 	for(x <- texto)
 	{
 		var lineList = get_next_t(x) 		
		for(d<-lineList)
		{	
			d.print
		}

		var pars = new Parser(lineList)
 	}
 
 	
 	//var tokenList = new ListBuffer[Token]()
 	
	def get_next_t(s: String) =
	{	
		var lineList = new ListBuffer[Token]()
		var pos = 0
		if((s indexOf "var")== 0)
		{
			lineList += new Token("VAR", 'v')
			pos = pos+3
		}
		var lista = s.toList
		while(pos< lista.length)
 		{
	 		
			var c = lista(pos)
			
			if(c.isDigit)
			{
				println("digit")
				lineList += new Token("DIGITO", c)

			}
			if(c=='.')
			{
				lineList  += new Token("PUNTO", c)
			}
			if(c == '+')
			{
				println("suma")

				lineList  += new Token("SUMA", c)
				
			}
			if(c == '-')
			{
				println("resta")
				lineList  += new Token("RESTA", c)
			}
			
			if(c == '*')
			{
				println("mult")
				lineList  += new Token("MULTIPLICACION", c)
			}
			
			if(c == '/')
			{
				println("div")
				lineList  += new Token("DIVISION", c)
			}
			if(c == '^')
			{
				println("potencia")
				lineList  += new Token("POTENCIA",c)
			}

			if(c == '%')
			{
				println("modulo")
				lineList  += new Token("MODULO", c)
			}
			if(c == '=')
			{
				println("Igualdad")
				lineList += new Token("IGUALDAD", c)
			}

			for(d<-variables)
			{
				if(c==d)
				{

					println("variable " + c)
					lineList += new Token("LETRA",c)
					
				}
			}
			pos = pos + 1	
		}
		lineList
		

	}
 }

 class Parser(tokens: ListBuffer[Token])
 {
 	println(CompilerFirstTry.variablef.contains('e'))

	var pos = 0;
	if(tokens(pos).getType =="VAR")
	{
		CompilerFirstTry.variablef += (tokens(pos+1).getVal -> 0)
		pos = pos +1
	}
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
			if(CompilerFirstTry.variablef.contains(tokens(pos).getVal))
			{
				x = true
			}
			else
			{
				println("Error Variable no definida")
				System.exit(1)
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
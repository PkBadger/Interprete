import scala.io.Source
import scala.util.control._
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map
import scala.math.pow

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


class Token(tipo : String, valor: String) {
	def print(){
	println(tipo + " " +valor)}
	def getType=tipo
	
	def getVal=valor

	override def toString = "sad"
	
}

class Interpreter(archivo: String){
	def print(){
		////println(archivo)
		val texto = archivo.split('\n').map(_.trim.filter(_ >= ' ')).mkString
		////println(texto)
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
		/*for(d<-lineList)
		{	
			d.print
		}*/
		lineList += new Token("ENDL" ,"FINAL")
		var pars = new Parser(lineList)
 	}
 
 	
 	//var tokenList = new ListBuffer[Token]()
 	
	def get_next_t(s: String) =
	{	
		var lineList = new ListBuffer[Token]()
		var pos = 0
		if((s indexOf "var")== 0)
		{
			lineList += new Token("VAR", "variable")
			pos = pos+3
		}
		var lista = s.toList
		
		while(pos< lista.length)
 		{
	 		////println("init pos " + pos + " Why" + lista(pos-1))

			var c = lista(pos)
			
			if(c.isDigit)
			{
				
				var numero = new StringBuilder
				var punto = false
				while(pos < lista.length &&lista(pos).isDigit)
				{
				numero += lista(pos)
				pos += 1
					if(!punto && pos < lista.length && lista(pos) == '.' )
					{
						numero += lista(pos)
						pos += 1
						punto = true
					}
					//println("pos " + pos + " ")

				}
				lineList += new Token("DIGITO", numero.toString)
			}
			if(c=='(')
			{
				lineList  += new Token("OPEN", c.toString)
				pos = pos + 1
			}
			if(c==')')
			{
				lineList  += new Token("CLOSE", c.toString)
				pos = pos + 1
			}
			if(c == '+')
			{
			

				lineList  += new Token("SUMA", c.toString)
				pos = pos + 1
				
			}
			if(c == '-')
			{
				
				lineList  += new Token("RESTA", c.toString)
				pos = pos + 1
			}
			
			if(c == '*')
			{
				
				lineList  += new Token("MULTIPLICACION", c.toString)
				pos = pos + 1
			}
			
			if(c == '/')
			{
			
				lineList  += new Token("DIVISION", c.toString)
				pos = pos + 1
			}
			if(c == '^')
			{
			
				lineList  += new Token("POTENCIA",c.toString)
				pos = pos + 1
			}

			if(c == '%')
			{
			
				lineList  += new Token("MODULO", c.toString)
				pos = pos + 1
			}
			if(c == '=')
			{
				
				lineList += new Token("IGUALDAD", c.toString)
				pos = pos + 1
			}

			for(d<-variables)
			{
				if(c==d)
				{

					
					lineList += new Token("LETRA",c.toString)
					pos = pos + 1
					
				}

			}
			if(c == ' ')
			{
				pos = pos + 1
			}

		
				
		}

		lineList
		

	}
 }

 class Parser(tokens: ListBuffer[Token])
 {
 	var multDiv = 0
 	var pot = 0
 	var m = 0
 	var result: Double = 0
 	var result2: Double = 0
 	var num: Double = 0
 	var numM: Double = 1
 	var vari : Char = '0'
 	var tama = tokens.length
	var pos = 0;
	var tokType: String = ""
	var flagtype = false
	var tokxType: String = "+"
	if(tokens(pos).getType =="VAR")
	{
		CompilerFirstTry.variablef += ((tokens(pos+1).getVal.toList)(0) -> 0)
		if(pos < tama -1)
		{pos = pos +1}
		
	}
	var flag = false
	/*for(t <- tokens)
	{
		if(!flag && t.getType == "DIGITO")
		{
			result += t.getVal.toDouble
			flag = true
		}
	}*/

	println(A)
	println(numM)
	println(result)
	
	
	////println(result)
//	tokens(pos)
	def A =
	{
		var thispos = pos
		//
//
//println("ENtro a A")
		var x = false
		if(Y && tokens(pos).getVal == "=")
		{////println("A")
			println("=")
			if(pos < tama -1){
			pos = pos + 1}
			if(E)
			{x=true
				}
		}
		else{ 
			pos = thispos
			if(E)
			{////println("A")
				x=true
				
			}
		}

		//
		if(tokType == "-")
		{	
			result -= num
			println("-Resultado de E " + result)
		}else if(tokType == "+")
		{
			result += num
			println("+Resultado de E " + result)
		}
		else if(tokType == "*")
		{
			result *= num
			println("*Resultado de E " + result)
		}
		else if(tokType == "/")
		{
			result /= num
			println("/Resultado de E " + result)
		}
		else if(tokType == "%")
		{
			result %= num
			println("%Resultado de E " + result)
		}
//
//println("Sale de A")
		x
	}

	/*def P =
	{
		//
//
//println("ENtro a P")
		var x = false
		if(tokens(pos).getVal == "=")
		{
		
		//println(tokens(pos).getVal)
			if(pos < tama -1)
			{pos = pos +1}
			if(E){
			x =true}
			
		}
		////
//
//println("Sale de P")
		x
	}*/

	def E:Boolean =
	{
		var thispos = pos
		var thisNum: Double = 0
		var thisRes = result
		var thisTok = tokxType
//println("ENtro a E")
		var x = false

		if(X && tokens(pos).getVal == "+")
		{////println("E")
			thisNum = num
			println("+Numero de X " + num +tokxType)
			if(tokxType == "+")
			{
				result +=num
			}else if(tokxType == "-")
			{
				result-=num
			}else if(tokxType == "*")
			{
				result *= num
			}else if(tokxType == "/")
			{
				result /= num
			}else if(tokxType == "%")
			{
				result %= num
			}else if(tokxType == "^")
			{
				pow(result,num)
			}
			println("+Resultado de X " + result +tokxType)
			tokxType = "+"
			if(pos < tama -1){
			pos = pos + 1}
			if(E)
			{x=true
				if(!flagtype){
				tokType = "+"
				flagtype=true }
				println("+NUmero de E " + num)
				
			}
		}else{
			pos = thispos
			result = thisRes
			tokxType = thisTok
			if(X && tokens(pos).getVal == "-")
			{////println( "E")
			println("-Numero de X " + num + " "+ tokxType )
				thisNum = num
				if(tokxType == "+")
				{
					result +=num
				}else if(tokxType == "-")
				{
					result-=num
				}
				println("-Resultado de X " + result +tokxType)
				//result -= num
				
			
			tokxType = "-"
				if(pos < tama -1){
				pos = pos + 1}
				if(E)
				{x=true
				if(!flagtype){
				tokType = "-"
				flagtype=true }
				//result -= thisNum
				println("-NUmero de E " + num)
				
				
				
				
				}
			}else{
				pos = thispos 
				result = thisRes
				tokxType = thisTok
				if(X)
				{
					
					x=true
				}
			}
		}
		//
//
//println("Sale de E")
		x
	}
	/*def Q = 
	{
		////
//println("ENtre a Q")
		var x = false
		if(tokens(pos).getVal == "+")
		{
		//println("Q")
		//println(tokens(pos).getVal)
			if(pos < tama -1)
			{pos = pos +1}
			if(E)
			{x=true}
			}else if(tokens(pos).getVal == "-")
			{//println("Q")
			//println(tokens(pos).getVal)
				if(pos < tama -1)
				{pos = pos +1}
				if(E)
				{x=true}
			}
			////
//
//println("Sale de Q")
		x
	}*/

	def X:Boolean =
	{
		//
//
//println("ENtro a X")
		var x = false
		if(Z && XP)
		{////println("X1")
			x=true
		}
		//
//
//println("Sale de X")
		x
	}
	def XP:Boolean =
	{
		//
//
//println("ENtro a XP ")
		var x = false
		if( tokens(pos).getVal == "*")
		{println("* " + result)
			if(tokxType == "+")
			{
				result +=num
			}else if(tokxType == "-")
			{
				result-=num
			}else if(tokxType == "*")
			{
				numM *= num
			}else if(tokxType == "/")
			{
				numM /= num
			}else if(tokxType == "%")
			{
				numM %= num
			}else if(tokxType == "^")
			{
				pow(numM,num)
			}
			tokxType = "*"

			if(pos < tama -1){
			pos = pos + 1}
			if(Z && XP)
			{x=true
				if(!flagtype)
				{tokType = "*"
				flagtype = true}
			}
		}else if( tokens(pos).getVal == "/")
		{////println("X1")
			if(tokxType == "+")
			{
				result +=num
			}else if(tokxType == "-")
			{
				result-=num
			}else if(tokxType == "*")
			{
				result *= num
			}else if(tokxType == "/")
			{
				result /= num
			}else if(tokxType == "%")
			{
				result %= num
			}else if(tokxType == "^")
			{
				pow(result,num)
			}
			tokxType = "/"
			println("/")
			if(pos < tama -1){
			pos = pos + 1}
			if(Z && XP)
			{x=true
			if(!flagtype)
				{tokType = "/"}}
		}else if( tokens(pos).getVal == "%")
		{////println("X1")
			if(tokxType == "+")
			{
				result +=num
			}else if(tokxType == "-")
			{
				result-=num
			}else if(tokxType == "*")
			{
				result *= num
			}else if(tokxType == "/")
			{
				result /= num
			}else if(tokxType == "%")
			{
				result %= num
			}else if(tokxType == "^")
			{
				pow(result,num)
			}
			tokxType = "%"
			println("%")
			if(pos < tama -1){
			pos = pos + 1}
			if(Z && XP)
			{x=true
			if(!flagtype)
				{tokType = "%"
				flagtype = true}}
			}else 
			{
				//println("EPSILON")
				x = true
			}
		//
//
//println("Sale de XP")
		x
	}
/*
	def R = 
	{
		////
//
//println("ENtro a R")
		var x = false
		if(tokens(pos).getVal == "*")
		{////println("R")
		//println(tokens(pos).getVal)
			if(pos < tama -1)
			{pos = pos +1}
			if(Z(0))
			{x=true}
			}else if(tokens(pos).getVal == "/" )
			{////println("R")
			//println(tokens(pos).getVal)
				if(pos < tama -1)
				pos = pos +1
				if(Z(0))
				{x=true}
			}else if(tokens(pos).getVal== "%")
			{////println("R")
			//println(tokens(pos).getVal)
				if(pos < tama -1)
				{pos = pos +1}
				if(Z(0))
				{x=true}
			}
			////
//
//println("Sale de R")
			x
	}
*/
	def Z:Boolean = 
	{
		//
//
//println("ENtro a Z")
		var x = false
		if(F && ZP)
		{
			x=true
		}
		//
//
//println("Sale de Z")
		x
	}

	def ZP:Boolean = 
	{
		//
//
//println("ENtro a ZP")
		var x = false
		if(tokens(pos).getVal == "^")
		{
			println("^")
			result = pow(result, num)
			if(pos < tama -1){
			pos = pos + 1}
			if(F && ZP)
			{
			x=true
			}
		}else
		{
			x=true
			//println("EPSILON")
		}
		//
//
//println("Sale de ZP")
		x
	}
/*
	def T =
	{
		////
//
//println("ENtro a T")
		var x = false
		if(tokens(pos).getVal == "^")
		{	////println("T")
		//println(tokens(pos).getVal)
			if(pos < tama -1)
			{pos = pos +1}
			if(F)
			{x=true}
		}
		////
//
//println("Sale de T")
		x
			
	}
*/
	def F =
	{
		//
//
//println("ENtro a F")
		var x = false
		if(N)
		{////println("F")
			x=true
		}else if(tokens(pos).getVal == "(")
		{////println("F")
			println("(")

			if(pos < tama -1){
			pos = pos + 1
		println(pos)}
			if(U)
			{x=true}
		}else if(Y)
		{////println("F")

			x=true
		}
		//
//
//println("Sale de F")
		x
	}

	def U=
	{
		//
//
//println("ENtro a U")
		var x = false
		if(E && tokens(pos).getVal == ")")
		{
			println(")")
			flagtype = false
		////println("U")
		//println(tokens(pos).getVal)
			
			if(pos < tama -1){
			pos = pos + 1
		println(pos)}
			x=true
			
		}
		//
//
//println("Sale de U")
		x
	}

	def Y =
	{	
		//
//
//println("ENtro a Y")
		var x = false
		if(tokens(pos).getType == "LETRA")
		{
			
			
			if(CompilerFirstTry.variablef.contains((tokens(pos).getVal.toList)(0)))
			{
				x = true
				println(tokens(pos).getVal)
			}
			else
			{
				//println("Error Variable no definida")
				System.exit(1)
			}
			
			//println(tokens(pos).getVal)
			
			if(pos < tama -1){
			pos = pos + 1}
			x = true
		}
		//
//
//println("Sale de Y")
		x	
	}

	def N:Boolean =
	{	
		//
//
//println("ENtro a N")
		var x = false
		
		if(tokens(pos).getType =="DIGITO")
		{

			println("N " +tokens(pos).getVal)

			if(pos < tama -1){
			num = tokens(pos).getVal.toDouble
			pos = pos + 1
			
			}
			x = true

		}
		//
//
//println("Sale de N")
		x
	}

 }

 /*
A -> Y P | E
P -> = E
E -> X Q | X
Q -> + E | - E 
X -> X R | Z
R -> * Z | / Z | % Z
Z -> Z T | F
T-> ^ F
F -> N | ( U | Y
U -> E )
Y -> a | b | c
N -> N 0 | N 1 | N 2 | N 3 | N 4 | N 5 | N 6 | N 7 | N 8 | N 9 | O | M .
O -> 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
M -> M 0 | M 1 | M 2 | M 3 | M 4 | M 5 | M 6 | M 7 | M 8 | M 9 | O*/


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
import scala.io.Source
import scala.util.control._
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map
import scala.math.pow

object Inter{ 
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
 	val igualdades = List("<=",">=","==","my","mn")
 	var pos = 0
 
 	while(pos< texto.length)
 	{
 		
 		var y = texto(pos)
 		val whiles = y indexOf("while(")
 		if(whiles == 0)
 		{
 			
 			val fina = y lastIndexOf(')')
			if(fina < 0)
			{
				println("Error de sintaxis")

			}
			else
			{	
				for(ig <-igualdades)
				{

					//println("Volvemos aqui2 ") 
					val inIg = y indexOf(ig)
					if(inIg >0)
					{
						var line = y.toList
						var lineList = get_next_t(y,whiles+6,inIg-1)
						var pars = new Parser(lineList)
		 				var res = pars.Get_result

		 				var lineList2 = get_next_t(y,inIg+2,fina-1)
		 				var pars2 = new Parser(lineList2)
		 				var res2 = pars2.Get_result
		 				if(ig == "<=")
		 				{
		 					if(res._2 <= res2._2)
		 					{
		 						
		 						if(pos + 1 < texto.length)
		 						{
		 							pos = Whiles(true,pos +1,ig,line(whiles+6),res2._2)	
		 						}

		 					}
		 					else 
		 					{
		 						if(pos + 1 < texto.length)
		 						{
		 							pos = Whiles(false,pos +1,ig,line(whiles+6),res2._2)		
		 						}
		 						
		 					}
		 				}else if(ig == ">=")
		 				{
		 					if(res._2 >= res2._2)
		 					{
		 						//println(">=")
		 						if(pos + 1 < texto.length)
		 						{
		 							pos = Whiles(true,pos +1,ig,line(whiles+6),res2._2)	
		 						}

		 					}
		 					else 
		 					{
		 						if(pos + 1 < texto.length)
		 						{
		 							pos = Whiles(false,pos +1,ig,line(whiles+6),res2._2)	
		 						}
		 					}

		 				}else if(ig == "==")
		 				{
		 					if(res._2 == res2._2)
		 					{
		 						
		 						if(pos + 1 < texto.length)
		 						{
		 							pos = Whiles(true,pos +1,ig,line(whiles+6),res2._2)	
		 						}

		 					}
		 					else 
		 					{
		 						if(pos + 1 < texto.length)
		 						{
		 							pos = Whiles(false,pos +1,ig,line(whiles+6),res2._2)	
		 						}
		 					}

		 				}else if(ig == "my")
		 				{
		 					if(res._2 > res2._2)
		 					{
		 						
		 						if(pos + 1 < texto.length)
		 						{
		 							pos = Whiles(true,pos +1,ig,line(whiles+6),res2._2)	
		 						}

		 					}
		 					else 
		 					{
		 						if(pos + 1 < texto.length)
		 						{
		 							pos = Whiles(false,pos +1,ig,line(whiles+6),res2._2)	
		 						}
		 					}

		 				}else if(ig == "mn")
		 				{
		 					if(res._2 < res2._2)
		 					{
		 						
		 						if(pos + 1 < texto.length)
		 						{	
		 							pos = Whiles(true,pos +1,ig,line(whiles+6),res2._2)			 							
		 						}

		 					}
		 					else 
		 					{
		 						if(pos + 1 < texto.length)
		 						{
		 							pos = Whiles(false,pos +1,ig,line(whiles+6),res2._2)	
		 						}
		 					}

		 				}
		 				
					}
				}
			}
 		}
 		val fors = y indexOf("for(")
 		if(fors == 0)
 		{
 			val fin = y lastIndexOf(')')
			if(fin < 0)
			{
				println("Error de sintaxis")
			}
			else
			{
	 			var to = y indexOf("to")
	 			if(to >0)
	 			{
	 				var lineList = get_next_t(y,fors+4,to-1)
					var pars = new Parser(lineList)
			 		var res = pars.Get_result

			 		var lineList2 = get_next_t(y,to+2,fin-1)
			 		var pars2 = new Parser(lineList2)
			 		var res2 = pars2.Get_result

			 		var times = res2._2 - res._2
			 		
			 		pos = ForTo(times.toInt,pos+1)
	 			}
	 			else
	 			{
	 				println("Error de sintaxis")
	 				System.exit(1)
	 			}
 			}
 		}
 		val ini = y indexOf("if(")
 		if(ini == 0)
 		{
 			val fin = y lastIndexOf(')')
			
			if(fin < 0)
			{
				println("Error de sintaxis")
				System.exit(1)
			}else
			{

				for(ig <-igualdades)
				{
					//println("Volvemos aqui2 ") 
					val inIg = y indexOf(ig)
					if(inIg >0)
					{
						var lineList = get_next_t(y,ini+3,inIg-1)
						var pars = new Parser(lineList)
		 				var res = pars.Get_result

		 				var lineList2 = get_next_t(y,inIg+2,fin-1)
		 				var pars2 = new Parser(lineList2)
		 				var res2 = pars2.Get_result
		 				if(ig == "<=")
		 				{
		 					if(res._2 <= res2._2)
		 					{
		 						
		 						if(pos + 1 < texto.length)
		 						{
		 							pos = Ifs(true,pos +1)
		 						}

		 					}
		 					else 
		 					{
		 						if(pos + 1 < texto.length)
		 						{
		 							pos = Ifs(false,pos +1)
		 						}
		 						
		 					}
		 				}else if(ig == ">=")
		 				{
		 					if(res._2 >= res2._2)
		 					{
		 						//println(">=")
		 						if(pos + 1 < texto.length)
		 						{
		 							pos = Ifs(true,pos +1)
		 						}

		 					}
		 					else 
		 					{
		 						if(pos + 1 < texto.length)
		 						{
		 							pos = Ifs(false,pos +1)
		 						}
		 					}

		 				}else if(ig == "==")
		 				{
		 					if(res._2 == res2._2)
		 					{
		 						
		 						if(pos + 1 < texto.length)
		 						{
		 							pos = Ifs(true,pos +1)
		 						}

		 					}
		 					else 
		 					{
		 						if(pos + 1 < texto.length)
		 						{
		 							pos = Ifs(false,pos +1)
		 						}
		 					}

		 				}else if(ig == "my")
		 				{
		 					if(res._2 > res2._2)
		 					{
		 						
		 						if(pos + 1 < texto.length)
		 						{
		 							pos = Ifs(true,pos +1)
		 						}

		 					}
		 					else 
		 					{
		 						if(pos + 1 < texto.length)
		 						{
		 							pos = Ifs(false,pos +1)
		 						}
		 					}

		 				}else if(ig == "mn")
		 				{
		 					if(res._2 < res2._2)
		 					{
		 						
		 						if(pos + 1 < texto.length)
		 						{
		 							pos = Ifs(true,pos +1)
		 						}

		 					}
		 					else 
		 					{
		 						if(pos + 1 < texto.length)
		 						{
		 							pos = Ifs(false,pos +1)
		 						}
		 					}

		 				}
		 				
					}
				}
				//pos = pos +1
			}
 			
 		}else{
 			if(pos < texto.length)
 			{y = texto(pos)
 		Exec(pos,0,y.length-1)
 		pos += 1}}

 	}

 	def Exec(pos:Int,ini:Int,fin:Int) 
 	{

 		var y = texto(pos)
 		var lineList = get_next_t(y,ini,fin)
 
 		var pars = new Parser(lineList)
 		var res = pars.Get_result
 	//	println(pos + " " + ini + " " + fin) 	
			if(!res._1)
			{
				println("Error de sintaxis (Parser)")
			}else{
			if(!res._3)
			{println(res._2)}
			}
			
 	}

 	def Whiles(bol:Boolean, pos1:Int,op:String,valI:Char,valD:Double)=
 	{
 		//println("1")
 		var pos = pos1
 		var posf = 0
 		//println("Variables " + valI)
 		while(pos < texto.length)
 		{
 			//println("21")
 			var end = texto(pos) indexOf("end")
 			if(end > -1)
 			{
 				posf = pos
 				pos = texto.length
 			}
 			pos +=1
 		}
 		pos = pos1
 		//result = Inter.variablef(y._3)
 		if(op == "<=")
 		{
 			while(Inter.variablef(valI) <= valD)
 			{
 				while(pos < posf)
	 			{
	 				
	 				Exec(pos,0,texto(pos).length - 1)
	 				pos +=1
	 			}
	 			pos = pos1
 			}
 		}else if(op == ">=")
 		{	
 			while(Inter.variablef(valI) >= valD)
 			{
 				while(pos < posf)
	 			{
	 				
	 				Exec(pos,0,texto(pos).length - 1)
	 				pos +=1
	 			}
	 			pos = pos1
 			}
 			
 		}else if(op == "==")
 		{
 			while(Inter.variablef(valI) == valD)
 			{
 				while(pos < posf)
	 			{
	 				
	 				Exec(pos,0,texto(pos).length - 1)
	 				pos +=1
	 			}
	 			pos = pos1
 			}	
 		}else if(op == "my")
 		{
 			while(Inter.variablef(valI) > valD)
 			{
 				while(pos < posf)
	 			{
	 				
	 				Exec(pos,0,texto(pos).length - 1)
	 				pos +=1
	 			}
	 			pos = pos1
 			}
 		}else if(op == "mn")
 		{
 			while(Inter.variablef(valI) < valD)
 			{
 				while(pos < posf)
	 			{
	 				
	 				Exec(pos,0,texto(pos).length - 1)
	 				pos +=1
	 			}
	 			pos = pos1
 			}
 		}

 		
 		posf +1
 	}
 	
 	def ForTo(times:Int,pos1:Int) =
 	{
 		var tim = 0
 		var pos = pos1
 		var posf = 0
 			while(pos < texto.length)
 			{ 
 				var end = texto(pos) indexOf("end")
 				if(end > -1)
 				{
 					posf = pos 
 					pos = texto.length
 				}
 				pos +=1
 			}
 			pos = pos1
 			while(tim < times)
 			{ 
	 			while(pos < posf)
	 			{
	 				
	 				Exec(pos,0,texto(pos).length - 1)
	 				pos +=1
	 			}
	 			tim+=1
	 			pos = pos1
 			}

 		posf + 1 

 	}
 
 	def Ifs(bol: Boolean,pos1:Int) =
 	{
 		var pos = pos1
 		var posf = pos
 		
 		if(bol)
 		{
 			
 			while(pos < texto.length)
 			{
 				var y = texto(pos)
 				val yIn = y indexOf("then")
 				if(yIn >= 0)
 				{   
 					
 					pos +=1
 					while (pos < texto.length)
 					{
 						if(((texto(pos)) indexOf("end")) < 0){
 							
	 						Exec(pos,0,texto(pos).length - 1)
	 						pos += 1
	 						

 						}
 						else
 						{
 							//println("Creo que entra aqui")
 							pos = pos + 1
 							if((texto(pos) indexOf ("else")) > -1)
 							{
 								//println("Hay un else")
 								while(pos< texto.length)
	 							{
	 								var end = texto(pos)
	 								if((end indexOf("end"))  > -1)
	 								{
	 									posf = pos +1 
	 									//println("despues " +texto(posf))
	 									pos = texto.length

	 								}
	 								pos += 1
	 							}
 							}else
 							{
 								posf = pos 
 								//println("despues " +texto(posf))
 								pos = texto.length
 							}
 							
 						
 						}
 					}
 				}
 			}
 		}
 		else
 		{
 			//println("Entra a else")
 			while(pos < texto.length)
 			{
 				var y = texto(pos)
 				val yIn = y indexOf("else")
 				if(yIn >= 0)
 				{
 					pos +=1
 					while (pos < texto.length)
 					{
 						if(((texto(pos)) indexOf("end")) < 0){
 							Exec(pos,0,texto(pos).length-1)
 							pos += 1
 						}
 						else
 						{
 							posf = pos + 1
 							pos = texto.length
 						}
 					}
 				}
 				else
 				{
 					pos = pos +1
 				}
 			}
 		}
 		posf
 	}


 	//var tokenList = new ListBuffer[Token]()

 	
	def get_next_t(s: String,ini : Int, fin:Int) =
	{	
		var lineList = new ListBuffer[Token]()
		lineList += new Token("ENDL" ,"FINAL")
		var pos = ini
	
		var lista = s.toList
		
		while(pos<= fin)
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
 	
	
	def Get_result = {
		
	 	val pos = tokens.length-1
	 	//println(pos)
	 	//println(tokens(pos).getVal)
	 	val a = A(pos)

		//println(a._1)
		(a._1,a._3,a._4)
	}

	def A(pos1 : Int) =
	{
		//println("Entra a A ")
		var pos = pos1
		var x = false //veridica si la linea es correcta
		var asig = false
		var result:Double = 0 //Resultados
		
		val e = E(pos)
		if(e._1 && tokens(e._2).getVal == "=")
		{
			
			val y = Y(e._2 -1) 
			if(y._1)
			{
				x = true
				Inter.variablef +=(y._3->e._3) //resultado devuelto por e

				asig = true
				pos = y._2 //posicion devuelta por e
			}
		}
		else if(e._1)
		{
			var e = E(pos)
			if(e._1)
			{
				x= true
				result = e._3
				pos = e._2 //posicion devuelta por e
			}
		}
		//println("Sale de A " + result)
		(x,pos,result,asig)

	}

	def E(pos1: Int):(Boolean,Int,Double) =
	{
		//println("Entra a E")
		var pos = pos1
		var q = false
		var result:Double = 0
		val x = X(pos)
		if(x._1) //verifica la parte de la X
		{
			if(tokens(x._2).getVal == "+")
			{
				val e = E(x._2 -1)
				if(e._1)
				{
					pos = e._2
					result = x._3 + e._3
					q = true
				}
			} else if(tokens(x._2).getVal == "-"){
				val e = E(x._2 -1)
				if(e._1)
				{
					pos = e._2
					result =e._3 -  x._3
					//println(x._3 +" - " + e._3 +" = " + result)
					q = true
				}
			}else
			{
				result = x._3
				pos = x._2
				q = true
			}
			
		}
		//println("Sale de E " + result)
		(q,pos,result)
	}
	
	def X(pos1: Int):(Boolean,Int,Double)  =
	{
		//println("Entra a X")
		var pos = pos1
		var x = false
		var result:Double = 0
		val z = Z(pos)
		if(z._1)
		{
			val xp = XP(z._2,z)
			if(xp._1)
			{
				x =true
				pos = xp._2
				result = xp._3
			}
		}
		//println("Sale de X " + result)
		(x,pos,result)	
	}
	def XP(pos1: Int, za: (Boolean,Int,Double) ):(Boolean,Int,Double)  = 
	{
		//println("Entra a XP")
		var pos = pos1
		var x = false
		var result:Double = 1
		if(tokens(pos).getVal == "*")
		{
			val z = Z(pos-1)
			if(z._1)
			{
				val xp = XP(z._2,z)
				if(xp._1)
				{
					x = true
					result = za._3 * xp._3
					pos =xp._2
				}
			}

		}else if(tokens(pos).getVal == "/"){
			val z = Z(pos-1)
			if(z._1)
			{
				val xp = XP(z._2,z)
				if(xp._1)
				{
					x = true
					result = xp._3 / za._3
					pos =xp._2
				}
			}

		

		}else if(tokens(pos).getVal == "%")
		{
			val z = Z(pos-1)
			if(z._1)
			{
				val xp = XP(z._2,z)
				if(xp._1)
				{
					x = true
					result = xp._3 % za._3 
					pos =xp._2
				}
			}
		}else
		{
			x=true
			result = za._3
		}
		//println("Sale de XP " + result)
		(x,pos,result)
	}

	def Z(pos1: Int):(Boolean,Int,Double) = 
	{
		//println("Entra a Z")
		var pos = pos1
		var x = false
		var result:Double = 0
		val f = F(pos)
		if(f._1)
		{
			val zp = ZP(f._2,f)
			if(zp._1)
			{
				x = true
				result = zp._3
				pos = zp._2
			}
		}
//println("Sale de Z " + result)
		(x,pos,result)
	}

	def ZP(pos1:Int,fa:(Boolean,Int,Double) ):(Boolean,Int,Double) =
	{
		//println("Entra a ZP")
		var pos = pos1
		var x = false
		var result:Double = 0
		if(tokens(pos).getVal == "^")
		{
			val f = F(pos-1)
			if(f._1)
			{
				val zp = ZP(f._2,f)
				if(zp._1)
				{
					x = true
					result = pow(zp._3,fa._3)
					pos =zp._2
				}
			}

		} else
		{
			x = true
			result = fa._3
		}
		//println("Sale de ZP " + result)
		(x,pos,result)

	}

	def F(pos1: Int)=
	{
		//println("Entra a F")
		var pos = pos1
		var x = false
		var result:Double = 0
		val n = N(pos)
		if(n._1)
		{
			result = n._3
			x = true
			pos = n._2
		} else if(tokens(pos).getVal == ")")
		{
			val u = U(pos-1)
			if(u._1)
			{
				result = u._3
				pos = u._2
				x = true
			}
		}else {
			val y = Y(pos)
			if(y._1)
			{
				
				pos = y._2
				x = true
				if(Inter.variablef.contains(y._3))
				{
					
					result = Inter.variablef(y._3)

				}else
				{	println("La variable no fue inicializada " +y._3)
					System.exit(1)}
				
				
			}	
		}
		//println("Sale de F " + result)
		(x,pos,result)
	}

	def U(pos1: Int)=
	{
		//println("Entra a U")
		var pos = pos1
		var x = false
		var result:Double = 0
		val e = E(pos)
		if(e._1)
		{
			if(tokens(e._2).getVal == "(")
			{
				x = true
				result = e._3
				pos = e._2 - 1
			}
		}
		//println("Sale de U " + result)
		(x,pos,result)
	}

	def Y(pos1: Int) =
	{	
		//println("Entra a Y")
		var pos = pos1
		var x = false
		var result:Double = 0
		var letra:Char = ' '
		
		if(tokens(pos).getType == "LETRA")
		{

			//print("Soy letra")
			
				x = true
				letra = (tokens(pos).getVal.toList)(0)
				pos = pos -1

			//println(tokens(pos).getVal)
			
			
		}
		//println("Sale de Y " )
		(x,pos,letra)
	}

	def N(pos1: Int):(Boolean,Int,Double)  =
	{	
		//println("Entra a N " + tokens(pos1).getVal)
		var pos = pos1
		var x = false
		var result:Double = 0
		if(tokens(pos).getType =="DIGITO")
		{
			
			result = tokens(pos).getVal.toDouble
			pos = pos - 1
			x = true

		}
		//println("Sale de N " + tokens(pos).getVal)
		(x,pos,result)
	}
 }


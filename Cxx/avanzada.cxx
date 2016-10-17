#include <iostream>
#include <string>
#include <map>
#include <set>

enum Estado { S0, S1, S2 };
/* S0 representa "Esperando Letra"
 * S1 representa "Esperando letra o numero"
 * S2 es el estado muerto
 */
enum Sigma  { Letra, Numero, Otro };


Sigma car_a_simb(char x) {
	if ((x >= 'a' && x <= 'z') || (x >= 'A' && x <= 'Z')) {
		return Letra;
	} else if (x >= '0' && x <= '9') {
		return Numero;
	} else {
		return Otro;
	}
}

bool es_identificador(const std::string& cadena) {
	         Estado  inicial = S0;
	std::set<Estado> finales;
	finales.insert(S1);
	
	std::map<Estado, std::map<Sigma, Estado> > delta;
	
	delta[S0][Letra ] = S1;
	delta[S0][Numero] = S2;
	delta[S0][Otro  ] = S2;
	
	delta[S1][Letra ] = S1;
	delta[S1][Numero] = S1;
	delta[S1][Otro  ] = S2;
	
	delta[S2][Letra ] = S2;
	delta[S2][Numero] = S2;
	delta[S2][Otro  ] = S2;
	
	Estado s = inicial;
	
	for (std::string::const_iterator i = cadena.begin(); i != cadena.end(); ++i) {
		s = delta[s][car_a_simb(*i)];
	}
	
	return (finales.find(s) != finales.end());
}



int main(int argc, char** argv) {
	
	std::string x = "";
	while (x != "1234") {
		std::cout << "Ingrese una cadena (cadena '1234' para salir): ";
		std::cin >> x;
		std::cout << (es_identificador(x) ? "Es" : "No es") << " un identificador" << std::endl;
	}
	
	return 0;
}

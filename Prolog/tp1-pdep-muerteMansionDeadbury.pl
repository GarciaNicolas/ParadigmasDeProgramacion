% Conocimientos

viveEnMansionDeadbury(tiaAgatha).
viveEnMansionDeadBury(mayordomo).
viveEnMansionDeadBury(charles).
viveEnMansionDeadbury(milhouse).

% Reglas

matoAAgatha(Persona) :-
    viveEnMansionDeadbury(Persona),
    not(esMasRicoQueAgatha(Persona)),
    esOdiadoPor(mayordomo,tiaAgatha).
matoAAgatha(Persona) :-
    viveEnMansionDeadbury(Persona),
    not(esMasRicoQueAgatha(Persona)),
    esOdiadoPor(charles,tiaAgatha).
matoAAgatha(Persona) :-
    viveEnMansionDeadbury(Persona),
    not(esMasRicoQueAgatha(Persona)),
    esOdiadoPor(tiaAgatha,tiaAgatha).


esMasRicoQueAgatha(Persona) :-
    not(esOdiadoPor(mayordomo,Persona)),
    viveEnMansionDeadbury(Persona).


% esOdiadoPor

esOdiadoPor(tiaAgatha, Persona) :-
    viveEnMansionDeadbury(Persona),
    Persona \= mayordomo.

esOdiadoPor(charles, Persona) :-
    not(esOdiadoPor(tiaAgatha,Persona)).

esOdiadoPor(mayordomo, Persona) :-
    esOdiadoPor(tiaAgatha,Persona).



% Resolución del caso: ?- matoAAgatha(Persona). Persona = tiaAgatha. (La tía Agatha se suicido)

% Alguien odia a Milhouse? ?- esOdiadoPor(_, milhouse). true. (Sí, alguien odia a milhouse)

% A quien odia Charles: ?- esOdiadoPor(charles,Persona). false. (Charles no odia a nadie)

% A quien odia Agatha: ?- esOdiadoPor(tiaAgatha,Persona). Persona = tiaAgatha. (La tía Agatha se odia)

/* Odiadores y Odiados
?- esOdiadoPor(Odiador, Odiado).
Odiador = Odiado, Odiado = tiaAgatha ;
Odiador = tiaAgatha, Odiado = milhouse ;
Odiador = mayordomo, Odiado = tiaAgatha ;
Odiador = mayordomo, Odiado = milhouse.
*/

% El mayordomo odia a alguien? esOdiadoPor(mayordomo, _). true. (Sí, el mayordomo odia a alguien) 

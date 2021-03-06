% Aquí va el código.
%MAGOS

%Mago(Nombre, statusSangre, caracteristicas, odiariaQuedar(Casa))
mago(harry, mestiza,   [coraje, amistoso, orgullo, inteligencia], odiariaQuedar(slytherin)).
mago(draco, pura,      [inteligencia, orgullo]                  , odiariaQuedar(hufflepuff)).
mago(hermione, impura, [inteligencia, orgullo, responsabilidad] ,   _).

%SOmbrero Seleccionador
%a).	Saber si una casa permite entrar a un mago,
% lo cual se cumple para cualquier mago y cualquier casa excepto en el caso de Slytherin, que no permite entrar a magos de sangre impura.


permite(slytherin, Mago):-
    mago(Mago, Sangre, _, _),
    Sangre \= impura. % no permite a gente con sangre impura

permite(Casa, Mago):-
    casa(Casa),
    mago(Mago, _, _, _),
    Casa \= slytherin.

%casas
casa(slytherin).
casa(hufflepuff).
casa(gryffindor).
casa(ravenclaw).


%b)Saber si un mago tiene el carácter apropiado para una casa, lo cual se cumple para cualquier mago si sus características incluyen todo lo que se busca para los integrantes de esa casa, independientemente de si la casa le permite la entrada.
%tieneCaracterApropiadoPara(Mago, Casa)

/*
tieneCaracterApropiadoPara(Mago, Casa):-
    mago(Mago, _, Caracteristicas, _),
    caracteristicasNecesarias(Casa, Caracteristicas).

caracteristicasNecesarias(gryffindor, Caracteristicas):-    member(coraje ,Caracteristicas).
caracteristicasNecesarias(slytherin, Caracteristicas):-     member(orgullo ,Caracteristicas),      member(inteligencia, Caracteristicas).
caracteristicasNecesarias(ravenclaw, Caracteristicas):-     member(inteligencia, Caracteristicas), member(responsabilidad, Caracteristicas).
caracteristicasNecesarias(hufflepuff, Caracteristicas):-    member(amistoso, Caracteristicas).

*/

%Sol Mas Linda hecha por profes

caracteristicaBuscada(gryffindor, coraje).
caracteristicaBuscada(slytherin, orgullo).
caracteristicaBuscada(slytherin, inteligencia).
caracteristicaBuscada(ravenclaw, inteligencia).
caracteristicaBuscada(ravenclaw, responsabilidad).
caracteristicaBuscada(hufflepuff, amistad).

tieneCaracteristica(Mago, Caracteristica):-
    mago(Mago, _, Caracteristicas, _),
    member(Caracteristica, Caracteristicas).
  
tieneCaracterApropiado2(Mago, Casa):-
    casa(Casa), mago(Mago, _, _, _),
    forall(
        caracteristicaBuscada(Casa, Caracteristica),
        tieneCaracteristica(Mago, Caracteristica)
    ).




%c)podriaQuedarEn(mago, Casa)
podriaQuedarEn(Mago, Casa):-
    tieneCaracterApropiado2(Mago, Casa),
    permite(Casa, Mago),
    not(mago(Mago, _, _, odiariaQuedar(Casa))).

podriaQuedarEn(hermione, gryffindor). %lo hackio!

%d)
%cadenaDeAmistades(Magos).
cadenaDeAmistades(Magos):-
    forall(
        member(Mago, Magos),
        amistoso(Mago)      %todos amistosos
        ),
    puedenEstarEnMismaCasa(Magos).
    
amistoso(Mago):-
    mago(Mago, _,Caracteristicas, _),
    member(amistoso,Caracteristicas).


%puedenEstarEnMismaCasa([Primero, Siguiente]):- %caso Base ERRONEOS
%    podriaQuedarEn(Primero, Casa),
%    podriaQuedarEn(Siguiente, Casa).
%
puedenEstarEnMismaCasa([Primero, Siguiente | MagosSiguientes]):-   %caso recursivo
    podriaQuedarEn(Primero, Casa),
    podriaQuedarEn(Siguiente, Casa),
    puedenEstarEnMismaCasa([Siguiente | MagosSiguientes]).

puedenEstarEnMismaCasa([_]).            %Casos Base Posta
puedenEstarEnMismaCasa([]).             %Casos Base



%Finalmenet quedaron seleccionados
esDe(hermione, gryffindor).
esDe(ron, gryffindor).
esDe(harry, gryffindor).
esDe(draco, slytherin).
esDe(luna, ravenclaw).

%Accion(Mago, Accion)
accion(harry, anduvoFueraDeLaCama). % resta 50
accion(hermione, fueA(tercerPiso)).
accion(hermione, fueA(seccionRestringidaBiblioteca)).
accion(harry, fueA(bosque)).
accion(harry, fueA(tercerPiso)).
accion(draco, fueA(mazmorras)).

accionPremiada(ron, ganarAjedrezMagico, 50).
accionPremiada(hermione, salvarAmigosDeMuerteHorrible, 50).
accionPremiada(harry, ganarleAVoldemor, 60).



%ACCIONES CLASIFICACION

%malasAcciones(Accion, PuntajeQueResta)

malasAcciones(anduvoFueraDeLaCama, -50).
malasAcciones(fueA(Lugar), Puntaje):-           sancionPorLugarProhibido(Lugar, Puntaje).

sancionPorLugarProhibido(bosque, -50).
sancionPorLugarProhibido(seccionRestringidaBiblioteca, -10).
sancionPorLugarProhibido(tercerPiso, -75).

%%buenasAcciones(Accion, puntajeQueSuma)
buenasAcciones(Accion, Puntaje):-   
    accionPremiada(_, Accion, Puntaje).



%1   a)buenAlumno
buenAlumno(Mago):-
    hizoAlgunaAccion(Mago,_),   %si hizo alguna acción
    forall(
        hizoAlgunaAccion(Mago, Accion),
        not(malasAcciones(Accion, _))).   % ninguna de las cosas que hizo se considera una mala acción

%hizoAlgunaAccion(Mago, Accion).
hizoAlgunaAccion(Mago, Accion):-            accion(Mago, Accion).
hizoAlgunaAccion(Mago, AccionPremiada):-   accionPremiada(Mago, AccionPremiada, _).

    %b)Saber si una acción es recurrente, que se cumple si más de un mago hizo esa misma acción.
%recurrente(Accion)

recurrente(Accion):-
   hizoAlgunaAccion(Mago1, Accion),
   hizoAlgunaAccion(Mago2, Accion),
   Mago1  \= Mago2.

%2 Saber cuál es el puntaje total de una casa, que es la suma de los puntos obtenidos por sus miembros.

/*puntajeTotal(Casa, PuntajeTotal, ListaPuntajes):-
    casa(Casa),
    forall(
        esDe(Mago,Casa), %miembro de la casa   
        findall(Puntos, puntosQueHizo(Mago, Puntos), ListaPuntajes)             
    ).
%    sum_list(ListaPuntajes, PuntajeTotal).
*/

puntajeTotal(Casa, PuntajeTotal):-
    casa(Casa),
    findall(PuntajeIndividual, puntosDeMiembroDeCasa(Casa, PuntajeIndividual), ListaPuntajes),
    sum_list(ListaPuntajes, PuntajeTotal).


puntosDeMiembroDeCasa(Casa,  Puntos):-
    esDe(Mago, Casa),                                            %miembro de la casa   
    puntosQueHizo(Mago, Puntos).

puntosQueHizo(Mago,PuntosNeg):-
        hizoAlgunaAccion(Mago, Accion),
        malasAcciones(Accion, PuntosNeg).

puntosQueHizo(Mago,PuntosNeg):-
        hizoAlgunaAccion(Mago, Accion),
        buenasAcciones(Accion, PuntosNeg).


%3 Saber cuál es la casa ganadora de la copa, que se verifica para aquella casa que haya obtenido una cantidad mayor de puntos que todas las otras.
casaGanadora(CasaGanadora):-
    puntajeTotal(CasaGanadora, PuntajeGanador),
    forall(
        puntajeTotal(Casa, Puntaje),
        PuntajeGanador >= Puntaje
    ).


%4	Queremos agregar la posibilidad de ganar puntos por responder preguntas en clase. La información que nos interesa de las respuestas en clase son: cuál fue la pregunta, cuál es la dificultad de la pregunta y qué profesor la hizo.
%respondioPregunta(Pregunta, Dificultad, QueProfeLaHizo).

accion(hermione, respondioPregunta(donde_se_encuentra_un_Bezoar, 20, snape)).
accion(hermione, respondioPregunta(como_hacer_levitar_una_Pluma, 25 , flitwick)).

buenasAcciones(respondioPregunta(Preg, Dificultad, snape), Puntos):-
    accion(_,  respondioPregunta(Preg, Dificultad, snape)),
    Puntos is Dificultad/2.

buenasAcciones(respondioPregunta(Preg, Dificultad, Prof), Dificultad):-
    accion(_, respondioPregunta(Preg, Dificultad, Prof)),
    Prof \= snape.


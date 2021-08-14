% Aquí va el código.
%MAGOS

%Mago(Nombre, statusSangre, caracteristicas, odiariaQuedar(Casa))
mago(harry, mestiza,   [coraje, amistoso, orgullo, inteligencia], odiariaQuedar(slytherin)).
mago(draco, pura,      [inteligencia, orgullo]                 ,   odiariaQuedar(hufflepuff)).
mago(hermione, impura, [inteligencia, orgullo, responsabilidad]    ,   _).

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

tieneCaracteristicasApropiadoPara(Mago, Casa):-
    mago(Mago, _, Caracteristicas, _),
    caracteristicasNecesarias(Casa, Caracteristicas).

caracteristicasNecesarias(gryffindor, Caracteristicas):-    member(coraje ,Caracteristicas).
caracteristicasNecesarias(slytherin, Caracteristicas):-     member(orgullo ,Caracteristicas), member(inteligencia, Caracteristicas).
caracteristicasNecesarias(ravenclaw, Caracteristicas):-     member(inteligencia, Caracteristicas), member(responsabilidad, Caracteristicas).
caracteristicasNecesarias(hufflepuff, Caracteristicas):-    member(amistoso, Caracteristicas).


%c)podriaQuedarEn(mago, Casa)
podriaQuedarEn(Mago, Casa):-
    tieneCaracteristicasApropiadoPara(Mago, Casa),
    permite(Casa, Mago),
    not(mago(Mago, _, _, odiariaQuedar(Casa))).

podriaQuedarEn(hermione, gryffindor).

%d)
%cadenaDeAmistades(Magos).
cadenaDeAmistades(Magos):-
    forall(
        member(Mago, Magos),
        amistoso(Mago)      %todos amistosos
        ),
    puedeEstarEnMismaCasa(Magos).
    
amistoso(Mago):-
    mago(Mago, _,Caracteristicas, _),
    member(amistoso,Caracteristicas).


puedeEstarEnMismaCasa([Primero, Siguiente]):- %caso Base
    podriaQuedarEn(Primero, Casa),
    podriaQuedarEn(Siguiente, Casa).

puedeEstarEnMismaCasa([Primero, Siguiente, Siguiente2 | _]):-   %caso recursivo
    podriaQuedarEn(Primero, Casa),
    podriaQuedarEn(Siguiente, Casa),
    puedeEstarEnMismaCasa([Siguiente, Siguiente2  | _]).


%Finalmenet quedaron seleccionados
esDe(hermione, gryffindor).
esDe(ron, gryffindor).
esDe(harry, gryffindor).
esDe(draco, slytherin).
esDe(luna, ravenclaw).

%Acciones

%anduvoFueraDeLaCama(Mago)
%fueA(Mago, Lugar)
%accionPremiada(Mago, Accion, Puntaje).

anduvoFueraDeLaCama(harry). % resta 50
fueA(hermione, tercerPiso).
fueA(hermione, seccionRestringidaBiblioteca).
fueA(harry, bosque).
fueA(harry, tercerPiso).
fueA(draco, mazmorras).

accionPremiada(ron, ganarAjedrezMagico(ron), 50).
accionPremiada(hermione, salvarAmigosDeMuerteHorrible(hermione), 50).
accionPremiada(harry, ganarleAVoldemor(harry), 60).


%ACCIONES CLASIFICACION

%malasAcciones(Mago, Accion(Mago), PuntajeQueResta)

malasAcciones(Mago, anduvoFueraDeLaCama(Mago), -50).
malasAcciones(Mago, fueA(Mago, Lugar), Puntaje):-           sancionPorLugarProhibido(Lugar, Puntaje).

sancionPorLugarProhibido(bosque, -50).
sancionPorLugarProhibido(seccionRestringidaBiblioteca, -10).
sancionPorLugarProhibido(tercerPiso, -75).

%buenasAcciones()




%1   a)buenAlumno
buenAlumno(Mago):-
    hizoAlgunaAccion(Mago,_),   %si hizo alguna acción
    forall(
        hizoAlgunaAccion(Mago, Accion),
        not(malasAcciones(Mago, Accion, _))).   % ninguna de las cosas que hizo se considera una mala acción

%hizoAlgunaAccion(Mago, Accion).
hizoAlgunaAccion(Mago, fueA(Mago, Lugar)):-            fueA(Mago, Lugar) .
hizoAlgunaAccion(Mago, anduvoFueraDeLaCama(Mago)):-    anduvoFueraDeLaCama(Mago).
hizoAlgunaAccion(Mago, accionPremiada(Mago, _, _)):-   accionPremiada(Mago, _, _).

    %b)Saber si una acción es recurrente, que se cumple si más de un mago hizo esa misma acción.
%recurrente(Accion)
recurrente(Accion):-
   hizoAlgunaAccion(Mago1, fueA()),
   hizoAlgunaAccion(Mago2, Accion),
   Mago1  \= Mago2.
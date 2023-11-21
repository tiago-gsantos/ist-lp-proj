% 106329 - Tiago Santos
:- set_prolog_flag(answer_write_options,[max_depth(0)]).
:- ['dados.pl'], ['keywords.pl'].


% Auxiliar

% Res --> lista dos periodos da ListaPeriodos e os respetivos semestres.
% Este predicado auxiliar itera pelos elementos da ListaPeriodos e, para cada
% periodo, chama os factos abaixo definidos, obtendo o respetivo semestre e
% adicionando-o a ListaCompleta. 
% No fim, e usado o list_to_set para retirar elementos repetidos, o que acontece
% se a ListaPeriodos tiver dois periodos do mesmo semestre.
atualizaListaPeriodos(ListaPeriodos, Res) :- 
  atualizaListaPeriodos(ListaPeriodos, ListaPeriodos, Res).

atualizaListaPeriodos([Periodo|RestoLista], ListaPeriodos, Res) :-
  periodoParaSemestre(Periodo, Semestre),
  append(ListaPeriodos, [Semestre], ListaCompleta),
  atualizaListaPeriodos(RestoLista, ListaCompleta, Res).

atualizaListaPeriodos([], ListaCompleta, Res) :-
  list_to_set(ListaCompleta, Res).

periodoParaSemestre(p1, p1_2).
periodoParaSemestre(p2, p1_2).
periodoParaSemestre(p3, p3_4).
periodoParaSemestre(p4, p3_4).


% Exercicios

% Eventos --> lista, ordenada e sem elementos repetidos, de IDs de eventos sem sala.
eventosSemSalas(Eventos) :- 
  setof(ID, X^Y^Z^evento(ID,X,Y,Z,semSala), Eventos).


% Eventos --> lista, ordenada e sem elementos repetidos, de IDs de eventos sem sala
% que decorrem no DiaSemana.
eventosSemSalasDiaSemana(DiaSemana, Eventos) :-
  findall(ID, 
         (eventosSemSalas(EventosSemSala), 
          member(ID, EventosSemSala),
          horario(ID, DiaSemana,_,_,_,_)),
          Eventos).


% Eventos --> lista, ordenada e sem elementos repetidos, de IDs de eventos sem sala
% nos periodos (e respetivos semestres) de ListaPeriodos.
eventosSemSalasPeriodo(ListaPeriodos, Eventos) :-
  atualizaListaPeriodos(ListaPeriodos, ListaPeriodosSemestres),
  findall(ID, 
         (eventosSemSalas(EventosSemSala),
          member(ID, EventosSemSala), 
          member(Periodo, ListaPeriodosSemestres), 
          horario(ID,_,_,_,_, Periodo)), 
          Eventos).


% Eventos --> lista, ordenada e sem elementos repetidos, de IDs dos eventos da
% ListaEventos que decorrem no Periodo (e no respetivo semestre).
organizaEventos(ListaEventos, Periodo, Eventos) :-
  periodoParaSemestre(Periodo, Semestre),
  organizaEventos(ListaEventos, Periodo, Semestre, [], Eventos).

organizaEventos([],_,_, ListaEventos, Eventos) :- 
  sort(ListaEventos, Eventos), !.

organizaEventos([Evento|RestoLista], Periodo, Semestre, ListaEventos, Eventos) :-
  (((horario(Evento,_,_,_,_,Periodo);
  horario(Evento,_,_,_,_,Semestre)),
  append([Evento], ListaEventos, ListaAtualizada));
  (ListaAtualizada = ListaEventos)), !,
  organizaEventos(RestoLista, Periodo, Semestre, ListaAtualizada, Eventos).


% Eventos --> lista, ordenada e sem elementos repetidos, de IDs dos eventos que
% tem duracao maxima menor ou igual a Duracao.
eventosMenoresQue(Duracao, Eventos) :-
  findall(ID, 
         (horario(ID,_,_,_,DuracaoEvento,_),
          DuracaoEvento =< Duracao),
          Eventos).


% Verdade se ID tiver duracao menor ou igual a Duracao.
eventosMenoresQueBool(ID, Duracao) :-
  horario(ID,_,_,_,DuracaoEvento,_),
  DuracaoEvento =< Duracao.


% ListaDisciplinas --> lista, ordenada alfabeticamente, do nome das disciplinas
% do Curso.
procuraDisciplinas(Curso, ListaDisciplinas) :-
  findall(Disciplina, 
         (turno(ID, Curso,_,_),
          evento(ID, Disciplina,_,_,_)),
          Disciplinas),
  sort(Disciplinas, ListaDisciplinas).


% Devolve uma lista de duas listas correspondentes a organizar a ListaDisciplinas
% de um certo Curso nas disciplinas de primeiro semestre e nas de segundo semestre
% (ambas ordenadas alfabeticamente e sem elementos repetidos).
organizaDisciplinas([],_,[[],[]]) :- !.

organizaDisciplinas([Disciplina|RestoLista], Curso, [PrimeiroSemestre, SS]) :- 
  evento(ID,Disciplina,_,_,_),
  turno(ID,Curso,_,_),
  horario(ID,_,_,_,_,Periodo),
  member(Periodo, [p1,p2,p1_2]),
  organizaDisciplinas(RestoLista, Curso, [PS,SS]),
  append([Disciplina],PS, Temp),
  sort(Temp, PrimeiroSemestre), !.

organizaDisciplinas([Disciplina|RestoLista], Curso, [PS, SegundoSemestre]) :- 
  evento(ID,Disciplina,_,_,_),
  turno(ID,Curso,_,_),
  horario(ID,_,_,_,_,Periodo),
  member(Periodo, [p3,p4,p3_4]),
  organizaDisciplinas(RestoLista, Curso, [PS,SS]),
  append([Disciplina],SS, Temp),
  sort(Temp, SegundoSemestre), !.


% TotalHoras --> numero de horas total dos eventos associados ao Curso, no Ano
% e Periodo dado (e respetivo semestre).
horasCurso(Periodo, Curso, Ano, TotalHoras) :-
  periodoParaSemestre(Periodo, Semestre),
  findall(ID, turno(ID, Curso, Ano, _), Temp),
  sort(Temp, ListaEventosCurso),
  findall(Duracao,
         (member(ID, ListaEventosCurso), 
          member(PeriodoSemestre, [Periodo, Semestre]), 
          horario(ID,_,_,_,Duracao,PeriodoSemestre)), 
          ListaDuracao),
  sum_list(ListaDuracao, TotalHoras).
  

% Evolucao --> lista ordenada de tuplos na forma (Ano, Periodo, NumHoras) em que
% NumHoras corresponde ao resultado de chamar horasCurso com o Curso, um Ano de
% 1 a 3 e um Periodo.
evolucaoHorasCurso(Curso, Evolucao) :-
  findall((Ano, Periodo, TotalHoras),
          (member(Ano, [1,2,3]),
          member(Periodo,[p1,p2,p3,p4]),
          horasCurso(Periodo, Curso, Ano, TotalHoras)),
          Evolucao).


% Horas -->  numero de horas sobrepostas entre o evento e o slot (intersecao 
% entre os dois intervalos). Se nao existir sobreposicao, ou seja, se o fim do
% evento for antes do inicio do slot ou o fim do slot for antes do inicio do
% evento, o predicado falha (return false).
ocupaSlot(InicioSlot, FimSlot, InicioEvento, FimEvento, Horas) :-
  FimEvento > InicioSlot,
  FimSlot > InicioEvento,
  max_list([InicioEvento, InicioSlot], X),
  min_list([FimEvento, FimSlot], Y),
  Horas is Y-X.


% SomaHoras --> numero horas ocupadas nas salas do TipoSala, entre a HoraInicio
% e HoraFim, no DiaSemana e no Periodo (e respetivo semestre).
numHorasOcupadas(Periodo, TipoSala, DiaSemana, HoraInicio, HoraFim, SomaHoras) :- 
  periodoParaSemestre(Periodo, Semestre),
  findall(Horas,
         (salas(TipoSala, ListaSalas),
          member(Sala, ListaSalas), 
          evento(ID,_,_,_,Sala),
          member(PeriodoSemestre, [Periodo, Semestre]),
          horario(ID, DiaSemana, InicioEvento, FimEvento,_,PeriodoSemestre),
          ocupaSlot(HoraInicio, HoraFim, InicioEvento, FimEvento, Horas)),
          ListaHoras),
  sum_list(ListaHoras, SomaHoras).


% Max --> numero de horas possiveis de ser ocupadas por salas do TipoSala entre
% HoraInicio e Hora Fim.
ocupacaoMax(TipoSala, HoraInicio, HoraFim, Max) :-
  salas(TipoSala, ListaSalas),
  length(ListaSalas, NumSalas),
  Max is NumSalas * (HoraFim - HoraInicio).


% Percentagem --> divisao de SomaHoras por Max multiplicada por 100.
percentagem(SomaHoras, Max, Percentagem) :-
  Percentagem is SomaHoras / Max * 100.


% Resultados --> lista ordenada de tuplos do tipo casosCriticos(DiaSemana, 
% TipoSala, Percentagem) em que estes argumentos tem em conta o intervalo de
% tempo entre HoraInicio e HoraFim, e a percentagem de ocupacao relativa a
% esses elementos esta acima de um dado valor critico (Threshold).
ocupacaoCritica(HoraInicio, HoraFim, Threshold, Resultados) :-
  findall(casosCriticos(DiaSemana, TipoSala, PercentagemArredondada),
         (member(DiaSemana, [segunda-feira, terca-feira, quarta-feira, quinta-feira, sexta-feira, sabado]),
          member(Periodo, [p1, p2, p3, p4]),
          salas(TipoSala,_),
          numHorasOcupadas(Periodo, TipoSala, DiaSemana, HoraInicio, HoraFim, SomaHoras),
          ocupacaoMax(TipoSala, HoraInicio, HoraFim, Max),
          percentagem(SomaHoras, Max, Percentagem),
          Percentagem > Threshold,
          PercentagemArredondada is ceiling(Percentagem)),
          ResultadosDesordenados),
  sort(ResultadosDesordenados, Resultados).


% OcupacaoMesa --> lista com tres listas que correspondem a unica disposicao possivel
% da mesa tendo em conta todas as pessoas da ListaPessoas e todas as restricoes
% da ListaRestricoes.
% Esta mesa e obtida atraves da eliminacao de possibilidades, ou seja, obtem-se
% uma lista de todas as distribuicoes possiveis da mesa e, uma a uma,
% vao se aplicando restricoes do ListaRestricoes usando o predicado auxiliar
% aplicaRestricoes/3 ate so sobrar uma. 
% A mesa e depois fromatada de maneira a ficar com as tres listas pedidas no enunciado. 
ocupacaoMesa(ListaNomes, Restricoes, OcupacaoMesa) :-
  findall(Permutacoes, permutation(ListaNomes, Permutacoes), Possibilidades),
  aplicaRestricoes(Restricoes, Possibilidades, Mesa),
  formataMesa(Mesa, OcupacaoMesa).
 
% Auxiliares
aplicaRestricoes([], [Mesa], Mesa).
aplicaRestricoes([Restricao|RestoLista], Possibilidades, Mesa) :-
  include(Restricao, Possibilidades, ListaReduzida),
  aplicaRestricoes(RestoLista, ListaReduzida, Mesa).
  
formataMesa([A,B,C,D,E,F,G,H], [[A,B,C],[D,E],[F,G,H]]).

% Restricoes
cab1(X, [_,_,_,X,_,_,_,_]).
cab2(X, [_,_,_,_,X,_,_,_]).

honra(X1, X2, [_,_,_,X1,_,X2,_,_]).
honra(X1, X2, [_,_,X2,_,X1,_,_,_]).

lado(X1, X2, [X1, X2|_]) :- !.
lado(X1, X2, [X2, X1|_]) :- !.
lado(X1, X2, [_|R]) :- lado(X1, X2, R).

naoLado(X1, X2, Mesa) :- not(lado(X1, X2, Mesa)). 

frente(X1, X2, [X1,_,_,_,_,X2|_]) :- !.
frente(X1, X2, [X2,_,_,_,_,X1|_]) :- !.
frente(X1, X2, [_|R]) :- frente(X1, X2, R).

naoFrente(X1, X2, Mesa) :- not(frente(X1, X2, Mesa)).
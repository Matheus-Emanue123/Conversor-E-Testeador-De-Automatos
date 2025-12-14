unit utils;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, tipos, automato;

function VerificarNaoDeterminismo(const A: TAutomato): Boolean;
function DetectarTipoAutomato(const A: TAutomato): String;
procedure ExibirDetalhes(const A: TAutomato; const Tipo: String);

implementation

function ChaveTransicao(const Estado, Simbolo: String): String;
begin
  Result := Estado + '|' + Simbolo;
end;

function VerificarNaoDeterminismo(const A: TAutomato): Boolean;
var
  Tabela: TTabela;
  Destinos: TStringSet;
  Estado, Simbolo: String;
  Chave: String;
begin
  if A.EstadosIniciais.Count > 1 then
    Exit(True);
  
  Tabela := ConstruirTabelaTransicoes(A);
  
  try
    for Destinos in Tabela.Values do
      if Destinos.Count > 1 then
      begin
        LiberarTabela(Tabela);
        Exit(True);
      end;
    
    for Estado in A.Estados do
      for Simbolo in A.Alfabeto do
      begin
        if Simbolo = '&' then Continue;
        Chave := ChaveTransicao(Estado, Simbolo);
        if not Tabela.ContainsKey(Chave) then
        begin
          LiberarTabela(Tabela);
          Exit(True);
        end;
      end;
  finally
    LiberarTabela(Tabela);
  end;
  
  Result := False;
end;

function DetectarTipoAutomato(const A: TAutomato): String;
begin
  if ConjuntoContem(A.Alfabeto, '&') then
    Result := 'AFN-Lambda'
  else if VerificarNaoDeterminismo(A) then
    Result := 'AFN'
  else
    Result := 'AFD';
end;

procedure ExibirDetalhes(const A: TAutomato; const Tipo: String);
var
  Trans: TTransicao;
  I: Integer;
begin
  WriteLn;
  WriteLn('==================================================');
  WriteLn('  DETALHES DO AUTOMATO (', Tipo, ')');
  WriteLn('==================================================');
  
  Write('Alfabeto: [');
  for I := 0 to A.Alfabeto.Count - 1 do
  begin
    Write(A.Alfabeto[I]);
    if I < A.Alfabeto.Count - 1 then Write(', ');
  end;
  WriteLn(']');
  
  Write('Estados: [');
  for I := 0 to A.Estados.Count - 1 do
  begin
    Write(A.Estados[I]);
    if I < A.Estados.Count - 1 then Write(', ');
  end;
  WriteLn(']');
  
  Write('Estado(s) inicial(is): [');
  for I := 0 to A.EstadosIniciais.Count - 1 do
  begin
    Write(A.EstadosIniciais[I]);
    if I < A.EstadosIniciais.Count - 1 then Write(', ');
  end;
  WriteLn(']');
  
  Write('Estado(s) final(is): [');
  for I := 0 to A.EstadosFinais.Count - 1 do
  begin
    Write(A.EstadosFinais[I]);
    if I < A.EstadosFinais.Count - 1 then Write(', ');
  end;
  WriteLn(']');
  
  WriteLn;
  WriteLn('Transicoes (', A.Transicoes.Count, '):');
  for Trans in A.Transicoes do
    WriteLn('  ', Trans.Origem, ' --', Trans.Simbolo, '--> ', Trans.Destino);
  
  WriteLn('==================================================');
end;

end.

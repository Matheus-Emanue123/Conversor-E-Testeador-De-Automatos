unit automato;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Generics.Collections, tipos;

type
  TTabela = specialize TDictionary<String, TStringSet>;

function ConstruirTabelaTransicoes(const A: TAutomato): TTabela;
function FechoLambda(Estados: TStringSet; Tabela: TTabela): TStringSet;
function AceitaPalavra(const A: TAutomato; const Palavra: String): Boolean;
procedure LiberarTabela(var T: TTabela);

implementation

function ChaveTransicao(const Estado, Simbolo: String): String;
begin
  Result := Estado + '|' + Simbolo;
end;

function ConstruirTabelaTransicoes(const A: TAutomato): TTabela;
var
  Trans: TTransicao;
  Chave: String;
  Destinos: TStringSet;
begin
  Result := TTabela.Create;
  
  for Trans in A.Transicoes do
  begin
    Chave := ChaveTransicao(Trans.Origem, Trans.Simbolo);
    
    if not Result.TryGetValue(Chave, Destinos) then
    begin
      Destinos := TStringSet.Create;
      Result.Add(Chave, Destinos);
    end;
    
    AdicionarAoConjunto(Destinos, Trans.Destino);
  end;
end;

function FechoLambda(Estados: TStringSet; Tabela: TTabela): TStringSet;
var
  Pilha: TStringSet;
  Estado, Proximo: String;
  Chave: String;
  Destinos: TStringSet;
begin
  Result := CopiarConjunto(Estados);
  Pilha := CopiarConjunto(Estados);
  
  try
    while Pilha.Count > 0 do
    begin
      Estado := Pilha[Pilha.Count - 1];
      Pilha.Delete(Pilha.Count - 1);
      
      Chave := ChaveTransicao(Estado, '&');
      
      if Tabela.TryGetValue(Chave, Destinos) then
      begin
        for Proximo in Destinos do
        begin
          if not ConjuntoContem(Result, Proximo) then
          begin
            AdicionarAoConjunto(Result, Proximo);
            Pilha.Add(Proximo);
          end;
        end;
      end;
    end;
  finally
    Pilha.Free;
  end;
end;

function AceitaPalavra(const A: TAutomato; const Palavra: String): Boolean;
var
  Tabela: TTabela;
  EstadosAtuais, ProximosEstados: TStringSet;
  Simbolo: Char;
  Estado, Proximo: String;
  Chave: String;
  Destinos: TStringSet;
  I: Integer;
begin
  Result := False;
  Tabela := ConstruirTabelaTransicoes(A);
  EstadosAtuais := FechoLambda(A.EstadosIniciais, Tabela);
  
  try
    for I := 1 to Length(Palavra) do
    begin
      Simbolo := Palavra[I];
      
      if not ConjuntoContem(A.Alfabeto, Simbolo) then
      begin
        EstadosAtuais.Free;
        LiberarTabela(Tabela);
        Exit(False);
      end;
      
      ProximosEstados := TStringSet.Create;
      
      for Estado in EstadosAtuais do
      begin
        Chave := ChaveTransicao(Estado, Simbolo);
        
        if Tabela.TryGetValue(Chave, Destinos) then
        begin
          for Proximo in Destinos do
            AdicionarAoConjunto(ProximosEstados, Proximo);
        end;
      end;
      
      EstadosAtuais.Free;
      
      if ProximosEstados.Count = 0 then
      begin
        ProximosEstados.Free;
        LiberarTabela(Tabela);
        Exit(False);
      end;
      
      EstadosAtuais := FechoLambda(ProximosEstados, Tabela);
      ProximosEstados.Free;
    end;
    
    for Estado in EstadosAtuais do
      if ConjuntoContem(A.EstadosFinais, Estado) then
      begin
        Result := True;
        Break;
      end;
  finally
    EstadosAtuais.Free;
    LiberarTabela(Tabela);
  end;
end;

procedure LiberarTabela(var T: TTabela);
var
  Conjunto: TStringSet;
begin
  for Conjunto in T.Values do
    Conjunto.Free;
  T.Free;
end;

end.

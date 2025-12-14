unit tipos;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Generics.Collections;

type
  TStringList = Classes.TStringList;
  TStringSet = specialize TList<String>;
  
  TTransicao = record
    Origem: String;
    Destino: String;
    Simbolo: String;
  end;
  
  TTransicaoList = specialize TList<TTransicao>;
  
  TAutomato = record
    Alfabeto: TStringSet;
    Estados: TStringSet;
    EstadosIniciais: TStringSet;
    EstadosFinais: TStringSet;
    Transicoes: TTransicaoList;
  end;

function CriarAutomato: TAutomato;
procedure LiberarAutomato(var A: TAutomato);
function ConjuntoContem(Conjunto: TStringSet; Item: String): Boolean;
procedure AdicionarAoConjunto(Conjunto: TStringSet; Item: String);
function ConjuntosIguais(A, B: TStringSet): Boolean;
function CopiarConjunto(Fonte: TStringSet): TStringSet;

implementation

function CriarAutomato: TAutomato;
begin
  Result.Alfabeto := TStringSet.Create;
  Result.Estados := TStringSet.Create;
  Result.EstadosIniciais := TStringSet.Create;
  Result.EstadosFinais := TStringSet.Create;
  Result.Transicoes := TTransicaoList.Create;
end;

procedure LiberarAutomato(var A: TAutomato);
begin
  A.Alfabeto.Free;
  A.Estados.Free;
  A.EstadosIniciais.Free;
  A.EstadosFinais.Free;
  A.Transicoes.Free;
end;

function ConjuntoContem(Conjunto: TStringSet; Item: String): Boolean;
var
  S: String;
begin
  Result := False;
  for S in Conjunto do
    if S = Item then
    begin
      Result := True;
      Exit;
    end;
end;

procedure AdicionarAoConjunto(Conjunto: TStringSet; Item: String);
begin
  if not ConjuntoContem(Conjunto, Item) then
    Conjunto.Add(Item);
end;

function ConjuntosIguais(A, B: TStringSet): Boolean;
var
  S: String;
begin
  if A.Count <> B.Count then
    Exit(False);
  
  for S in A do
    if not ConjuntoContem(B, S) then
      Exit(False);
  
  Result := True;
end;

function CopiarConjunto(Fonte: TStringSet): TStringSet;
var
  S: String;
begin
  Result := TStringSet.Create;
  for S in Fonte do
    Result.Add(S);
end;

end.

unit conversoes;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Generics.Collections, tipos, automato;

function AFNParaAFD(const AFN: TAutomato): TAutomato;
function AFDParaAFN(const AFD: TAutomato): TAutomato;
function AFNParaAFNLambda(const AFN: TAutomato): TAutomato;
function AFDParaAFNLambda(const AFD: TAutomato): TAutomato;

implementation

type
  TEstadoAFD = class
    Estados: TStringSet;
    Nome: String;
    constructor Create;
    destructor Destroy; override;
  end;

constructor TEstadoAFD.Create;
begin
  Estados := TStringSet.Create;
end;

destructor TEstadoAFD.Destroy;
begin
  Estados.Free;
  inherited;
end;

function ChaveTransicao(const Estado, Simbolo: String): String;
begin
  Result := Estado + '|' + Simbolo;
end;

function ConjuntoParaNome(Estados: TStringSet): String;
var
  Lista: TStringList;
  S: String;
  I: Integer;
begin
  if Estados.Count = 0 then
    Exit('{}');
  
  Lista := TStringList.Create;
  try
    for S in Estados do
      Lista.Add(S);
    Lista.Sort;
    
    Result := '{';
    for I := 0 to Lista.Count - 1 do
    begin
      Result := Result + Lista[I];
      if I < Lista.Count - 1 then
        Result := Result + ',';
    end;
    Result := Result + '}';
  finally
    Lista.Free;
  end;
end;

function AFNParaAFD(const AFN: TAutomato): TAutomato;
var
  TabelaAFN: TTabela;
  EstadoInicialAFD: TStringSet;
  Fila: specialize TList<TEstadoAFD>;
  Processados: specialize TList<String>;
  EstadoAtual: TEstadoAFD;
  NomeAtual: String;
  Simbolo, EstadoAFN, Proximo: String;
  ProximoEstado: TStringSet;
  Destinos: TStringSet;
  Chave: String;
  Trans: TTransicao;
  Temp: TEstadoAFD;
begin
  Result := CriarAutomato;
  TabelaAFN := ConstruirTabelaTransicoes(AFN);
  EstadoInicialAFD := FechoLambda(AFN.EstadosIniciais, TabelaAFN);
  
  Fila := specialize TList<TEstadoAFD>.Create;
  Processados := specialize TList<String>.Create;
  
  try
    Temp := TEstadoAFD.Create;
    Temp.Estados.Free;
    Temp.Estados := EstadoInicialAFD;
    Temp.Nome := ConjuntoParaNome(EstadoInicialAFD);
    Fila.Add(Temp);
    
    while Fila.Count > 0 do
    begin
      EstadoAtual := Fila[0];
      Fila.Delete(0);
      NomeAtual := EstadoAtual.Nome;
      
      if Processados.IndexOf(NomeAtual) >= 0 then
      begin
        EstadoAtual.Free;
        Continue;
      end;
      
      Processados.Add(NomeAtual);
      AdicionarAoConjunto(Result.Estados, NomeAtual);
      
      for Proximo in EstadoAtual.Estados do
        if ConjuntoContem(AFN.EstadosFinais, Proximo) then
        begin
          AdicionarAoConjunto(Result.EstadosFinais, NomeAtual);
          Break;
        end;
      
      for Simbolo in AFN.Alfabeto do
      begin
        if Simbolo = '&' then Continue;
        
        ProximoEstado := TStringSet.Create;
        
        for EstadoAFN in EstadoAtual.Estados do
        begin
          Chave := ChaveTransicao(EstadoAFN, Simbolo);
          if TabelaAFN.TryGetValue(Chave, Destinos) then
            for Proximo in Destinos do
              AdicionarAoConjunto(ProximoEstado, Proximo);
        end;
        
        if ProximoEstado.Count > 0 then
        begin
          Temp := TEstadoAFD.Create;
          Temp.Estados.Free;
          Temp.Estados := FechoLambda(ProximoEstado, TabelaAFN);
          Temp.Nome := ConjuntoParaNome(Temp.Estados);
          
          Trans.Origem := NomeAtual;
          Trans.Destino := Temp.Nome;
          Trans.Simbolo := Simbolo;
          Result.Transicoes.Add(Trans);
          
          if Processados.IndexOf(Temp.Nome) < 0 then
            Fila.Add(Temp)
          else
            Temp.Free;
        end;
        
        ProximoEstado.Free;
      end;
      
      EstadoAtual.Free;
    end;
    
    for Simbolo in AFN.Alfabeto do
      if Simbolo <> '&' then
        AdicionarAoConjunto(Result.Alfabeto, Simbolo);
    
    NomeAtual := ConjuntoParaNome(EstadoInicialAFD);
    AdicionarAoConjunto(Result.EstadosIniciais, NomeAtual);
    
  finally
    for Temp in Fila do
      Temp.Free;
    Fila.Free;
    Processados.Free;
    LiberarTabela(TabelaAFN);
  end;
end;

function AFDParaAFN(const AFD: TAutomato): TAutomato;
var
  S: String;
  Trans: TTransicao;
begin
  Result := CriarAutomato;
  
  for S in AFD.Alfabeto do
    Result.Alfabeto.Add(S);
  for S in AFD.Estados do
    Result.Estados.Add(S);
  for S in AFD.EstadosIniciais do
    Result.EstadosIniciais.Add(S);
  for S in AFD.EstadosFinais do
    Result.EstadosFinais.Add(S);
  for Trans in AFD.Transicoes do
    Result.Transicoes.Add(Trans);
end;

function AFNParaAFNLambda(const AFN: TAutomato): TAutomato;
var
  S, EstadoInicial, Simbolo, Destino: String;
  Trans, NovoTrans: TTransicao;
  Tabela: TTabela;
  Destinos: TStringSet;
  Chave: String;
  Existe: Boolean;
  T: TTransicao;
begin
  Result := CriarAutomato;
  
  for S in AFN.Alfabeto do
    Result.Alfabeto.Add(S);
  for S in AFN.Estados do
    Result.Estados.Add(S);
  for S in AFN.EstadosIniciais do
    Result.EstadosIniciais.Add(S);
  for S in AFN.EstadosFinais do
    Result.EstadosFinais.Add(S);
  for Trans in AFN.Transicoes do
    Result.Transicoes.Add(Trans);
  
  if not ConjuntoContem(Result.Alfabeto, '&') then
    AdicionarAoConjunto(Result.Alfabeto, '&');
  
  Tabela := ConstruirTabelaTransicoes(AFN);
  
  try
    for EstadoInicial in AFN.EstadosIniciais do
      for Simbolo in AFN.Alfabeto do
      begin
        Chave := ChaveTransicao(EstadoInicial, Simbolo);
        if Tabela.TryGetValue(Chave, Destinos) then
          for Destino in Destinos do
          begin
            Existe := False;
            for T in Result.Transicoes do
              if (T.Origem = EstadoInicial) and (T.Destino = Destino) and (T.Simbolo = '&') then
              begin
                Existe := True;
                Break;
              end;
            
            if not Existe then
            begin
              NovoTrans.Origem := EstadoInicial;
              NovoTrans.Destino := Destino;
              NovoTrans.Simbolo := '&';
              Result.Transicoes.Add(NovoTrans);
            end;
          end;
      end;
  finally
    LiberarTabela(Tabela);
  end;
end;

function AFDParaAFNLambda(const AFD: TAutomato): TAutomato;
var
  AFN: TAutomato;
begin
  AFN := AFDParaAFN(AFD);
  try
    Result := AFNParaAFNLambda(AFN);
  finally
    LiberarAutomato(AFN);
  end;
end;

end.

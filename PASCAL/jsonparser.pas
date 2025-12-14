unit jsonparser;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, tipos;

function CarregarAutomatoJSON(const NomeArquivo: String): TAutomato;
procedure SalvarAutomatoJSON(const NomeArquivo: String; const A: TAutomato);

implementation

function ExtrairArray(const Linha: String): TStringList;
var
  S, Item: String;
  InString: Boolean;
  I: Integer;
begin
  Result := TStringList.Create;
  S := Trim(Linha);
  
  if Pos('[', S) = 0 then Exit;
  
  S := Copy(S, Pos('[', S) + 1, Length(S));
  S := Copy(S, 1, Pos(']', S) - 1);
  
  Item := '';
  InString := False;
  
  for I := 1 to Length(S) do
  begin
    if S[I] = '"' then
    begin
      InString := not InString;
      if not InString and (Item <> '') then
      begin
        Result.Add(Item);
        Item := '';
      end;
    end
    else if InString then
      Item := Item + S[I];
  end;
end;

function CarregarAutomatoJSON(const NomeArquivo: String): TAutomato;
var
  F: TextFile;
  Linha, Key: String;
  Lista: TStringList;
  I: Integer;
  Trans: TTransicao;
  InTransicoes: Boolean;
  TransArray: TStringList;
begin
  Result := CriarAutomato;
  AssignFile(F, NomeArquivo);
  Reset(F);
  InTransicoes := False;
  
  try
    while not EOF(F) do
    begin
      ReadLn(F, Linha);
      Linha := Trim(Linha);
      
      if Pos('"alfabeto"', Linha) > 0 then
      begin
        Lista := ExtrairArray(Linha);
        for I := 0 to Lista.Count - 1 do
          AdicionarAoConjunto(Result.Alfabeto, Lista[I]);
        Lista.Free;
      end
      else if Pos('"estados"', Linha) > 0 then
      begin
        if Pos('"estados_iniciais"', Linha) = 0 then
        begin
          Lista := ExtrairArray(Linha);
          for I := 0 to Lista.Count - 1 do
            AdicionarAoConjunto(Result.Estados, Lista[I]);
          Lista.Free;
        end;
      end
      else if Pos('"estados_iniciais"', Linha) > 0 then
      begin
        Lista := ExtrairArray(Linha);
        for I := 0 to Lista.Count - 1 do
          AdicionarAoConjunto(Result.EstadosIniciais, Lista[I]);
        Lista.Free;
      end
      else if Pos('"estados_finais"', Linha) > 0 then
      begin
        Lista := ExtrairArray(Linha);
        for I := 0 to Lista.Count - 1 do
          AdicionarAoConjunto(Result.EstadosFinais, Lista[I]);
        Lista.Free;
      end
      else if Pos('"transicoes"', Linha) > 0 then
        InTransicoes := True
      else if InTransicoes and (Pos('[', Linha) > 0) and (Pos(']', Linha) > 0) then
      begin
        TransArray := ExtrairArray(Linha);
        if TransArray.Count = 3 then
        begin
          Trans.Origem := TransArray[0];
          Trans.Destino := TransArray[1];
          Trans.Simbolo := TransArray[2];
          Result.Transicoes.Add(Trans);
        end;
        TransArray.Free;
      end;
    end;
  finally
    CloseFile(F);
  end;
end;

procedure SalvarAutomatoJSON(const NomeArquivo: String; const A: TAutomato);
var
  F: TextFile;
  I: Integer;
  S: String;
  Lista: TStringList;
  Trans: TTransicao;
begin
  AssignFile(F, NomeArquivo);
  Rewrite(F);
  
  try
    WriteLn(F, '{');
    
    Write(F, '    "alfabeto": [');
    for I := 0 to A.Alfabeto.Count - 1 do
    begin
      Write(F, '"' + A.Alfabeto[I] + '"');
      if I < A.Alfabeto.Count - 1 then Write(F, ', ');
    end;
    WriteLn(F, '],');
    
    Write(F, '    "estados": [');
    for I := 0 to A.Estados.Count - 1 do
    begin
      Write(F, '"' + A.Estados[I] + '"');
      if I < A.Estados.Count - 1 then Write(F, ', ');
    end;
    WriteLn(F, '],');
    
    Write(F, '    "estados_iniciais": [');
    for I := 0 to A.EstadosIniciais.Count - 1 do
    begin
      Write(F, '"' + A.EstadosIniciais[I] + '"');
      if I < A.EstadosIniciais.Count - 1 then Write(F, ', ');
    end;
    WriteLn(F, '],');
    
    Write(F, '    "estados_finais": [');
    for I := 0 to A.EstadosFinais.Count - 1 do
    begin
      Write(F, '"' + A.EstadosFinais[I] + '"');
      if I < A.EstadosFinais.Count - 1 then Write(F, ', ');
    end;
    WriteLn(F, '],');
    
    WriteLn(F, '    "transicoes": [');
    for I := 0 to A.Transicoes.Count - 1 do
    begin
      Trans := A.Transicoes[I];
      Write(F, '        ["' + Trans.Origem + '", "' + Trans.Destino + '", "' + Trans.Simbolo + '"]');
      if I < A.Transicoes.Count - 1 then
        WriteLn(F, ',')
      else
        WriteLn(F);
    end;
    WriteLn(F, '    ]');
    WriteLn(F, '}');
  finally
    CloseFile(F);
  end;
end;

end.

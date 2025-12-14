program AutomatosPascal;

{$mode objfpc}{$H+}

uses
  SysUtils, tipos, jsonparser, automato, conversoes, utils;

var
  AutomatoAtual: TAutomato;
  TipoAtual: String;
  AutomatoCarregado: Boolean;
  Opcao: String;
  Caminho, Palavra: String;
  Resultado: Boolean;
  Novo: TAutomato;

procedure MenuPrincipal;
begin
  WriteLn;
  WriteLn('==================================================');
  WriteLn('    CONVERSAO E TESTE DE AUTOMATOS');
  WriteLn('==================================================');
  
  if AutomatoCarregado then
  begin
    WriteLn('Automato carregado: ', TipoAtual);
    WriteLn('Estados: ', AutomatoAtual.Estados.Count, ' | Alfabeto: ', AutomatoAtual.Alfabeto.Count);
  end
  else
    WriteLn('Nenhum automato carregado');
  
  WriteLn;
  WriteLn('--- MENU ---');
  WriteLn('1. Carregar automato de arquivo JSON');
  WriteLn('2. Converter automato');
  WriteLn('3. Testar palavra (terminal)');
  WriteLn('4. Testar palavras (arquivo)');
  WriteLn('5. Salvar automato atual');
  WriteLn('6. Exibir detalhes do automato');
  WriteLn('0. Sair');
  WriteLn('--------------------------------------------------');
  Write('Escolha uma opcao: ');
end;

procedure CarregarAutomato;
var
  SR: TSearchRec;
  Contador: Integer;
begin
  WriteLn;
  WriteLn('--- Carregar Automato ---');
  WriteLn('Arquivos JSON disponiveis:');
  
  Contador := 0;
  if FindFirst('../automatos/*.json', faAnyFile, SR) = 0 then
  begin
    repeat
      if (SR.Attr and faDirectory) = 0 then
      begin
        Inc(Contador);
        WriteLn('  ', Contador, '. ../automatos/', SR.Name);
      end;
    until FindNext(SR) <> 0;
    FindClose(SR);
  end;
  
  if Contador = 0 then
    WriteLn('  (Nenhum arquivo encontrado)');
  
  WriteLn;
  Write('Digite o caminho do arquivo: ');
  ReadLn(Caminho);
  
  try
    if AutomatoCarregado then
      LiberarAutomato(AutomatoAtual);
    
    AutomatoAtual := CarregarAutomatoJSON(Caminho);
    TipoAtual := DetectarTipoAutomato(AutomatoAtual);
    AutomatoCarregado := True;
    WriteLn('✓ Automato carregado com sucesso! Tipo: ', TipoAtual);
  except
    on E: Exception do
      WriteLn('✗ Erro ao carregar: ', E.Message);
  end;
end;

procedure MenuConversao;
begin
  WriteLn;
  WriteLn('--- Converter Automato ---');
  WriteLn('Tipo atual: ', TipoAtual);
  WriteLn;
  WriteLn('Opcoes de conversao:');
  WriteLn('1. AFN → AFD');
  WriteLn('2. AFD → AFN');
  WriteLn('3. AFN → AFN-Lambda');
  WriteLn('4. AFD → AFN-Lambda');
  WriteLn('0. Voltar');
  Write('Escolha: ');
  ReadLn(Opcao);
  
  case Opcao of
    '1':
    begin
      WriteLn('Convertendo AFN → AFD...');
      Novo := AFNParaAFD(AutomatoAtual);
      LiberarAutomato(AutomatoAtual);
      AutomatoAtual := Novo;
      TipoAtual := 'AFD';
      WriteLn('✓ Conversao concluida!');
    end;
    '2':
    begin
      WriteLn('Convertendo AFD → AFN...');
      Novo := AFDParaAFN(AutomatoAtual);
      LiberarAutomato(AutomatoAtual);
      AutomatoAtual := Novo;
      TipoAtual := 'AFN';
      WriteLn('✓ Conversao concluida!');
    end;
    '3':
    begin
      WriteLn('Convertendo AFN → AFN-Lambda...');
      Novo := AFNParaAFNLambda(AutomatoAtual);
      LiberarAutomato(AutomatoAtual);
      AutomatoAtual := Novo;
      TipoAtual := 'AFN-Lambda';
      WriteLn('✓ Conversao concluida!');
    end;
    '4':
    begin
      WriteLn('Convertendo AFD → AFN-Lambda...');
      Novo := AFDParaAFNLambda(AutomatoAtual);
      LiberarAutomato(AutomatoAtual);
      AutomatoAtual := Novo;
      TipoAtual := 'AFN-Lambda';
      WriteLn('✓ Conversao concluida!');
    end;
    '0': ;
  else
    WriteLn('✗ Opcao invalida!');
  end;
end;

procedure TestarPalavra;
begin
  WriteLn;
  WriteLn('--- Modo de Teste de Palavras ---');
  WriteLn('Tipo do automato: ', TipoAtual);
  WriteLn('Digite ''SAIR'' para voltar ao menu principal');
  WriteLn;
  
  repeat
    Write('Digite a palavra a ser testada (ou SAIR): ');
    ReadLn(Palavra);
    
    if UpperCase(Palavra) = 'SAIR' then
    begin
      WriteLn('Voltando ao menu principal...');
      Break;
    end;
    
    Resultado := AceitaPalavra(AutomatoAtual, Palavra);
    
    if Resultado then
      WriteLn('✓ A palavra "', Palavra, '" e ACEITA pelo automato!')
    else
      WriteLn('✗ A palavra "', Palavra, '" e REJEITADA pelo automato!');
    WriteLn;
  until False;
end;

procedure TestarPalavrasArquivo;
var
  ArquivoPalavras: TextFile;
  CaminhoPalavras, PalavraAtual: String;
  Aceitas, Rejeitadas, Total: Integer;
  Resultado: Boolean;
begin
  WriteLn;
  Write('Digite o caminho do arquivo de palavras: ');
  ReadLn(CaminhoPalavras);
  
  if not FileExists(CaminhoPalavras) then
  begin
    WriteLn('✗ Erro: Arquivo "', CaminhoPalavras, '" nao encontrado!');
    Exit;
  end;
  
  WriteLn;
  WriteLn('--- Testando palavras do arquivo: ', CaminhoPalavras, ' ---');
  WriteLn;
  
  Aceitas := 0;
  Rejeitadas := 0;
  Total := 0;
  
  AssignFile(ArquivoPalavras, CaminhoPalavras);
  Reset(ArquivoPalavras);
  
  try
    while not Eof(ArquivoPalavras) do
    begin
      ReadLn(ArquivoPalavras, PalavraAtual);
      Inc(Total);
      
      Resultado := AceitaPalavra(AutomatoAtual, PalavraAtual);
      
      if Resultado then
      begin
        WriteLn('✓ ''', PalavraAtual, ''' → ACEITA');
        Inc(Aceitas);
      end
      else
      begin
        WriteLn('✗ ''', PalavraAtual, ''' → REJEITADA');
        Inc(Rejeitadas);
      end;
    end;
    
    WriteLn;
    WriteLn('Resumo: ', Aceitas, ' aceitas, ', Rejeitadas, ' rejeitadas (Total: ', Total, ')');
  finally
    CloseFile(ArquivoPalavras);
  end;
end;

procedure SalvarAutomato;
begin
  WriteLn;
  Write('Digite o caminho para salvar (ex: saida.json): ');
  ReadLn(Caminho);
  
  try
    SalvarAutomatoJSON(Caminho, AutomatoAtual);
    WriteLn('✓ Automato salvo em "', Caminho, '"!');
  except
    on E: Exception do
      WriteLn('✗ Erro ao salvar: ', E.Message);
  end;
end;

begin
  AutomatoCarregado := False;
  
  repeat
    MenuPrincipal;
    ReadLn(Opcao);
    
    case Opcao of
      '1': CarregarAutomato;
      
      '2':
      begin
        if not AutomatoCarregado then
          WriteLn('✗ Carregue um automato primeiro!')
        else
          MenuConversao;
      end;
      
      '3':
      begin
        if not AutomatoCarregado then
          WriteLn('✗ Carregue um automato primeiro!')
        else
          TestarPalavra;
      end;
      
      '4':
      begin
        if not AutomatoCarregado then
          WriteLn('✗ Carregue um automato primeiro!')
        else
          TestarPalavrasArquivo;
      end;
      
      '5':
      begin
        if not AutomatoCarregado then
          WriteLn('✗ Carregue um automato primeiro!')
        else
          SalvarAutomato;
      end;
      
      '6':
      begin
        if not AutomatoCarregado then
          WriteLn('✗ Carregue um automato primeiro!')
        else
          ExibirDetalhes(AutomatoAtual, TipoAtual);
      end;
      
      '0':
      begin
        WriteLn;
        WriteLn('Encerrando programa... Ate logo!');
      end;
      
    else
      WriteLn('✗ Opcao invalida! Tente novamente.');
    end;
    
  until Opcao = '0';
  
  if AutomatoCarregado then
    LiberarAutomato(AutomatoAtual);
end.

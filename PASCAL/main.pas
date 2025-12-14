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
  WriteLn('3. Testar palavra');
  WriteLn('4. Salvar automato atual');
  WriteLn('5. Exibir detalhes do automato');
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
  WriteLn('--- Testar Palavra ---');
  Write('Digite a palavra a ser testada (deixe vazio para palavra vazia): ');
  ReadLn(Palavra);
  
  WriteLn('Testando no ', TipoAtual, '...');
  
  Resultado := AceitaPalavra(AutomatoAtual, Palavra);
  
  if Resultado then
    WriteLn('✓ A palavra "', Palavra, '" e ACEITA pelo automato!')
  else
    WriteLn('✗ A palavra "', Palavra, '" e REJEITADA pelo automato!');
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
          SalvarAutomato;
      end;
      
      '5':
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

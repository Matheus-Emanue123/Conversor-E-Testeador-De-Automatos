# Sistema de Autômatos em Pascal

Implementação completa do sistema de conversão e teste de autômatos em Pascal (Free Pascal Compiler).

## Estrutura do Projeto

```
PASCAL/
├── tipos.pas         - Tipos e estruturas de dados
├── jsonparser.pas    - Parser JSON para carregar/salvar autômatos
├── automato.pas      - Operações básicas com autômatos
├── conversoes.pas    - Conversões AFN↔AFD, AFN-Lambda
├── utils.pas         - Funções auxiliares
└── main.pas          - Programa principal
```

## Requisitos

- **Lazarus IDE** (inclui Free Pascal Compiler)
- Sistema operacional: Windows, Linux ou macOS

## 📥 Passo a Passo - Instalação

### Windows (RECOMENDADO)

**1. Baixar Lazarus IDE**
- Acesse: https://sourceforge.net/projects/lazarus/files/Lazarus%20Windows%2064%20bits/Lazarus%203.4/
- Baixe: `lazarus-3.4-fpc-3.2.2-win64.exe` (≈250 MB)

**2. Instalar**
- Execute o instalador
- Clique em "Next" até finalizar
- Instalação padrão em `C:\lazarus`

**3. Verificar Instalação**
- Abra PowerShell
- Digite: `C:\lazarus\fpc\3.2.2\bin\x86_64-win64\fpc.exe -h`
- Se aparecer a ajuda do compilador, está instalado! ✅

### Linux (Debian/Ubuntu)
```bash
sudo apt-get update
sudo apt-get install fpc
```

### macOS
```bash
brew install fpc
```

## 🔨 Passo a Passo - Compilação

### Opção 1: Linha de Comando (Apresentação)

**1. Abra o PowerShell**

**2. Configure o PATH e compile:**
```powershell
$env:PATH = "C:\lazarus\fpc\3.2.2\bin\x86_64-win64;$env:PATH"
cd "C:\Users\hecla\Desktop\CEFET\6° periodo\Linguagens Formais e Autômatos\PASCAL"
fpc main.pas
```

**3. Execute:**
```powershell
.\main.exe
```

### Opção 2: Script Automático

**Dê duplo clique em:**
```
compilar.bat
```

Pronto! Compila e executa automaticamente.

### Opção 3: Lazarus IDE (Gráfico)

**1. Abra o Lazarus**

**2. Abrir Projeto:**
- File → Open → Selecione `main.pas`

**3. Compilar e Executar:**
- Pressione `F9`
- Ou clique em "Run" → "Run"

## ▶️ Execução

Após compilar, você terá o arquivo `main.exe`.

**Execute:**
```bash
# Windows
.\main.exe

# Linux/macOS
./main
```

## Módulos

### tipos.pas
**Define as estruturas de dados principais**
- `TAutomato`: Record com alfabeto, estados, transições
- `TTransicao`: Record para transições
- `TStringSet`: Lista genérica de strings
- Funções auxiliares para manipulação de conjuntos

### jsonparser.pas
**Parser JSON simples**
- `CarregarAutomatoJSON`: Lê autômato de arquivo JSON
- `SalvarAutomatoJSON`: Salva autômato em JSON
- `ExtrairArray`: Extrai arrays de strings do JSON

### automato.pas
**Operações fundamentais**
- `ConstruirTabelaTransicoes`: Cria tabela de transições
- `FechoLambda`: Calcula fecho-λ
- `AceitaPalavra`: Verifica se palavra é aceita

### conversoes.pas
**Conversões entre tipos**
- `AFNParaAFD`: Construção de subconjuntos
- `AFDParaAFN`: Conversão trivial
- `AFNParaAFNLambda`: Adiciona λ-transições
- `AFDParaAFNLambda`: Composição de conversões

### utils.pas
**Utilidades**
- `VerificarNaoDeterminismo`: Detecta não-determinismo
- `DetectarTipoAutomato`: Identifica tipo (AFD/AFN/AFN-Lambda)
- `ExibirDetalhes`: Mostra informações do autômato

## Características do Pascal

### Diferenças com Python

1. **Tipagem Estática**: Todos os tipos devem ser declarados
2. **Gerenciamento Manual**: Records e listas precisam ser liberados
3. **Units**: Equivalente aos módulos Python
4. **Generics**: `specialize TList<String>` para listas tipadas

### Estruturas Usadas

```pascal
type
  TAutomato = record
    Alfabeto: TStringSet;
    Estados: TStringSet;
    EstadosIniciais: TStringSet;
    EstadosFinais: TStringSet;
    Transicoes: TTransicaoList;
  end;
```

## Formato JSON

Mesmo formato do Python:

```json
{
    "alfabeto": ["a", "b"],
    "estados": ["q0", "q1"],
    "estados_iniciais": ["q0"],
    "estados_finais": ["q1"],
    "transicoes": [
        ["q0", "q1", "a"]
    ]
}
```

## Menu do Sistema

```
1. Carregar automato de arquivo JSON
2. Converter automato
3. Testar palavra
4. Salvar automato atual
5. Exibir detalhes do automato
0. Sair
```

## 🎬 Exemplo de Uso Completo

### Do Zero até Execução:

```powershell
# 1. Configurar ambiente
$env:PATH = "C:\lazarus\fpc\3.2.2\bin\x86_64-win64;$env:PATH"

# 2. Ir para a pasta
cd "C:\Users\hecla\Desktop\CEFET\6° periodo\Linguagens Formais e Autômatos\PASCAL"

# 3. Compilar
fpc main.pas

# 4. Executar
.\main.exe
```

### Interação com o Programa:

```
==================================================
    CONVERSAO E TESTE DE AUTOMATOS
==================================================
Nenhum automato carregado

--- MENU ---
1. 🚀 Opções Avançadas de Compilação

```bash
# Compilação padrão
fpc main.pas

# Máxima otimização (mais rápido)
fpc -O3 main.pas

# Modo debug (para encontrar erros)
fpc -g main.pas

# Compilação silenciosa (sem avisos)
fpc main.pas 2>nul
```

## ⚠️ Troubleshooting (Resolução de Problemas)

### Erro: "fpc não é reconhecido"
**Solução:** Configure o PATH antes de compilar:
```powershell
$env:PATH = "C:\lazarus\fpc\3.2.2\bin\x86_64-win64;$env:PATH"
```

### Erro: "Error: Identifier not found Result"
**Solução:** Os arquivos já incluem `{$mode objfpc}{$H+}`. Se o erro persistir, recompile assim:
```bash
fpc -Mobjfpc -Sh main.pas
```

### Programa não abre
**Solução:** Execute pelo PowerShell:
```powershell
cd PASCAL
.\main.exe
```

### Não encontra arquivos JSON
**Solução:** Use caminho relativo correto:
- Se estiver na pasta `PASCAL`: `../automatos/teste.json`
- Se estiver na pasta raiz: `automatos/teste.json
--- Carregar Automato ---
Arquivos JSON disponiveis:
  1. ../automatos/afd_comeca_termina_mesmo.json
  2. ../automatos/afd_par_a.json
  3. ../automatos/afd_vazio.json
  4. ../automatos/afn_contem_substring.json
  5. ../automatos/afn_lambda_complexo.json
  6. ../automatos/afn_multiplos_iniciais.json
  7. ../automatos/teste.json
  8. ../automatos/testeafn.json
  9. ../automatos/testelambda.json

Digite o caminho do arquivo: ../automatos/teste.json
✓ Automato carregado com sucesso! Tipo: AFD

Escolha uma opcao: 3
Digite a palavra: ab
✓ A palavra "ab" e ACEITA pelo automato!

Escolha uma opcao: 2
--- Converter Automato ---
Tipo atual: AFD
Opcoes de conversao:
1. AFN → AFD
2. AFD → AFN
3. AFN → AFN-Lambda
4. AFD → AFN-Lambda
0. Voltar
Escolha: 3
Convertendo AFD → AFN-Lambda...
✓ Conversao concluida!

Escolha uma opcao: 4
Digite o caminho para salvar: saida.json
✓ Automato salvo em "saida.json"!
```

## Vantagens da Implementação Pascal

1. **Performance**: Código compilado nativo, muito rápido
2. **Portabilidade**: Funciona em Windows, Linux, macOS
3. **Aprendizado**: Mostra paradigmas diferentes de programação
4. **Memória**: Gerenciamento explícito de recursos
5. **Tipos**: Segurança em tempo de compilação

## Compilação com Otimização

```bash
# Máxima otimização
fpc -O3 -Mobjfpc -Sh main.pas

# Debug mode
fpc -g -Mobjfpc -Sh main.pas
```

## Pontos Extras! 🎯

Esta implementação demonstra:
- Conversão entre paradigmas (Python → Pascal)
- Conhecimento de múltiplas linguagens
- Adaptação de algoritmos para diferentes contextos
- Gerenciamento manual de memória
- Programação estruturada clássica

import json
from typing import List, Set, Dict, Tuple

class Automato:
    """Classe base para representar um autômato"""
    
    def __init__(self, alfabeto: List[str], estados: List[str], 
                 estados_iniciais: List[str], estados_finais: List[str],
                 transicoes: List[List[str]]):
        self.alfabeto = set(alfabeto)
        self.estados = set(estados)
        self.estados_iniciais = set(estados_iniciais)
        self.estados_finais = set(estados_finais)
        # transicoes: lista de [estado_origem, estado_destino, simbolo]
        self.transicoes = transicoes
        
    @classmethod
    def carregar_json(cls, caminho: str):
        """Carrega um autômato de um arquivo JSON"""
        with open(caminho, 'r', encoding='utf-8') as f:
            dados = json.load(f)
        return cls(
            dados['alfabeto'],
            dados['estados'],
            dados['estados_iniciais'],
            dados['estados_finais'],
            dados['transicoes']
        )
    
    def salvar_json(self, caminho: str):
        """Salva o autômato em um arquivo JSON"""
        dados = {
            'alfabeto': sorted(list(self.alfabeto)),
            'estados': sorted(list(self.estados)),
            'estados_iniciais': sorted(list(self.estados_iniciais)),
            'estados_finais': sorted(list(self.estados_finais)),
            'transicoes': self.transicoes
        }
        with open(caminho, 'w', encoding='utf-8') as f:
            json.dump(dados, f, indent=4, ensure_ascii=False)     
        
    def construir_tabela_transicoes(self) -> Dict[Tuple[str, str], Set[str]]:
        """
        Constrói um dicionário para acesso rápido às transições
        Retorna: {(estado_origem, simbolo): {estados_destino}}
        """
        tabela = {}
        for origem, destino, simbolo in self.transicoes:
            chave = (origem, simbolo)
            if chave not in tabela:
                tabela[chave] = set()
            tabela[chave].add(destino)
        return tabela
    
    def fecho_lambda(self, estados: Set[str], tabela: Dict) -> Set[str]:
        """
        Calcula o fecho-lambda (epsilon-closure) de um conjunto de estados
        Retorna todos os estados alcançáveis por transições lambda (&)
        """
        fecho = set(estados)  # Começa com os estados atuais
        pilha = list(estados)  # Estados a processar
        
        while pilha:
            estado = pilha.pop()
            # Busca transições lambda deste estado
            if (estado, '&') in tabela:
                for proximo in tabela[(estado, '&')]:
                    if proximo not in fecho:
                        fecho.add(proximo)
                        pilha.append(proximo)
        
        return fecho
    
    def aceita_palavra(self, palavra: str, eh_afd: bool = False) -> bool:
        """
        Verifica se uma palavra é aceita pelo autômato
        palavra: string a ser testada (use "" para palavra vazia)
        eh_afd: True se for AFD, False se for AFN/AFN-Lambda
        """
        tabela = self.construir_tabela_transicoes()
        
        # Estado inicial (com fecho-lambda se necessário)
        estados_atuais = self.fecho_lambda(self.estados_iniciais, tabela)
        
        # Processa cada símbolo da palavra
        for simbolo in palavra:
            if simbolo not in self.alfabeto:
                return False  # Símbolo inválido
            
            proximos_estados = set()
            
            # Para cada estado atual, vê para onde pode ir
            for estado in estados_atuais:
                if (estado, simbolo) in tabela:
                    proximos_estados.update(tabela[(estado, simbolo)])
            
            # Se não há próximos estados, palavra rejeitada
            if not proximos_estados:
                return False
            
            # Aplica fecho-lambda nos novos estados
            estados_atuais = self.fecho_lambda(proximos_estados, tabela)
        
        # Aceita se algum estado atual é final
        return bool(estados_atuais & self.estados_finais)                   


def afn_para_afd(afn: Automato) -> Automato:
    """
    Converte um AFN (ou AFN-Lambda) para AFD usando o algoritmo de construção de subconjuntos
    """
    tabela_afn = afn.construir_tabela_transicoes()
    
    # Estado inicial do AFD: fecho-lambda dos estados iniciais do AFN
    estado_inicial_afd = frozenset(afn.fecho_lambda(afn.estados_iniciais, tabela_afn))
    
    # Estruturas para o AFD
    estados_afd = set()
    transicoes_afd = []
    estados_finais_afd = set()
    
    # Fila de estados a processar (BFS)
    fila = [estado_inicial_afd]
    processados = set()
    
    while fila:
        estado_atual = fila.pop(0)  # Pega o primeiro da fila
        
        if estado_atual in processados:
            continue
        
        processados.add(estado_atual)
        estados_afd.add(estado_atual)
        
        # Se algum estado do conjunto é final, este estado do AFD é final
        if estado_atual & afn.estados_finais:
            estados_finais_afd.add(estado_atual)
        
        # Para cada símbolo do alfabeto (exceto lambda)
        for simbolo in afn.alfabeto:
            if simbolo == '&':  # Ignora transições lambda
                continue
            
            # Calcula próximo estado: união de todos os destinos
            proximo_estado = set()
            for estado_afn in estado_atual:
                if (estado_afn, simbolo) in tabela_afn:
                    proximo_estado.update(tabela_afn[(estado_afn, simbolo)])
            
            # Aplica fecho-lambda no resultado
            proximo_estado = afn.fecho_lambda(proximo_estado, tabela_afn)
            
            if proximo_estado:  # Se há transição
                proximo_estado = frozenset(proximo_estado)
                
                # Adiciona transição ao AFD
                transicoes_afd.append([
                    converter_estado_afd_para_nome(estado_atual),
                    converter_estado_afd_para_nome(proximo_estado),
                    simbolo
                ])
                
                # Adiciona novo estado à fila se ainda não foi processado
                if proximo_estado not in processados:
                    fila.append(proximo_estado)
    
    # Converte nomes dos estados para formato legível
    estados_afd_nomes = {converter_estado_afd_para_nome(e) for e in estados_afd}
    estados_finais_afd_nomes = {converter_estado_afd_para_nome(e) for e in estados_finais_afd}
    estado_inicial_afd_nome = converter_estado_afd_para_nome(estado_inicial_afd)
    
    # Remove lambda do alfabeto se existir
    alfabeto_afd = afn.alfabeto - {'&'}
    
    return Automato(
        alfabeto=list(alfabeto_afd),
        estados=list(estados_afd_nomes),
        estados_iniciais=[estado_inicial_afd_nome],
        estados_finais=list(estados_finais_afd_nomes),
        transicoes=transicoes_afd
    )


def converter_estado_afd_para_nome(estado_frozenset) -> str:
    """
    Converte um frozenset de estados para um nome legível
    Ex: frozenset({'q0', 'q1'}) -> '{q0,q1}'
    """
    if not estado_frozenset:
        return '{}'
    return '{' + ','.join(sorted(estado_frozenset)) + '}'


def afd_para_afn(afd: Automato) -> Automato:
    """
    Converte AFD para AFN (conceitualmente são equivalentes, apenas muda a representação)
    Um AFD já É um AFN, só copiamos a estrutura
    """
    return Automato(
        alfabeto=list(afd.alfabeto),
        estados=list(afd.estados),
        estados_iniciais=list(afd.estados_iniciais),
        estados_finais=list(afd.estados_finais),
        transicoes=afd.transicoes.copy()
    )


def afn_para_afn_lambda(afn: Automato) -> Automato:
    """
    Converte AFN para AFN-Lambda adicionando transições lambda estratégicas
    Adiciona transições lambda entre estados para criar caminhos alternativos
    """
    # Copia a estrutura original
    novos_estados = list(afn.estados)
    novas_transicoes = afn.transicoes.copy()
    novo_alfabeto = list(afn.alfabeto)
    
    # Adiciona '&' ao alfabeto se não existir
    if '&' not in novo_alfabeto:
        novo_alfabeto.append('&')
    
    # Estratégia: adicionar transições lambda que criam não-determinismo
    # Adiciona lambda do estado inicial para alguns estados alcançáveis
    tabela = afn.construir_tabela_transicoes()
    
    # Para cada estado inicial, adiciona lambda para estados alcançáveis em 1 passo
    for estado_inicial in afn.estados_iniciais:
        for simbolo in afn.alfabeto:
            if (estado_inicial, simbolo) in tabela:
                for destino in tabela[(estado_inicial, simbolo)]:
                    # Adiciona transição lambda do inicial para destino
                    # Só se não criar redundância
                    if [estado_inicial, destino, '&'] not in novas_transicoes:
                        novas_transicoes.append([estado_inicial, destino, '&'])
    
    return Automato(
        alfabeto=novo_alfabeto,
        estados=novos_estados,
        estados_iniciais=list(afn.estados_iniciais),
        estados_finais=list(afn.estados_finais),
        transicoes=novas_transicoes
    )


def afd_para_afn_lambda(afd: Automato) -> Automato:
    """
    Converte AFD diretamente para AFN-Lambda
    Combina as conversões: AFD -> AFN -> AFN-Lambda
    """
    afn = afd_para_afn(afd)
    return afn_para_afn_lambda(afn)


def testar_palavra_terminal(automato: Automato) -> None:
    """
    Testa uma palavra digitada pelo usuário no terminal
    """
    print("\n--- Testar Palavra ---")
    palavra = input("Digite a palavra a ser testada (deixe vazio para palavra vazia): ")
    
    # Detecta tipo do autômato
    eh_lambda = '&' in automato.alfabeto
    tem_nao_determinismo = verificar_nao_determinismo(automato)
    
    if eh_lambda:
        tipo = "AFN-Lambda"
    elif tem_nao_determinismo:
        tipo = "AFN"
    else:
        tipo = "AFD"
    
    print(f"Testando no {tipo}...")
    
    resultado = automato.aceita_palavra(palavra)
    
    if resultado:
        print(f"✓ A palavra '{palavra}' é ACEITA pelo autômato!")
    else:
        print(f"✗ A palavra '{palavra}' é REJEITADA pelo autômato!")


def testar_palavras_arquivo(automato: Automato, caminho_arquivo: str) -> None:
    """
    Testa múltiplas palavras de um arquivo (uma por linha)
    """
    print(f"\n--- Testando palavras do arquivo: {caminho_arquivo} ---")
    
    try:
        with open(caminho_arquivo, 'r', encoding='utf-8') as f:
            palavras = [linha.strip() for linha in f.readlines()]
    except FileNotFoundError:
        print(f"Erro: Arquivo '{caminho_arquivo}' não encontrado!")
        return
    
    if not palavras:
        print("Arquivo vazio!")
        return
    
    print(f"Total de palavras a testar: {len(palavras)}\n")
    
    aceitas = 0
    rejeitadas = 0
    
    for palavra in palavras:
        resultado = automato.aceita_palavra(palavra)
        status = "ACEITA" if resultado else "REJEITADA"
        simbolo = "✓" if resultado else "✗"
        
        print(f"{simbolo} '{palavra}' → {status}")
        
        if resultado:
            aceitas += 1
        else:
            rejeitadas += 1
    
    print(f"\nResumo: {aceitas} aceitas, {rejeitadas} rejeitadas")


def verificar_nao_determinismo(automato: Automato) -> bool:
    """
    Verifica se o autômato tem não-determinismo
    Retorna True se for AFN, False se for AFD
    """
    tabela = automato.construir_tabela_transicoes()
    
    # Se tem mais de um estado inicial, é não-determinístico
    if len(automato.estados_iniciais) > 1:
        return True
    
    # Verifica se algum (estado, símbolo) leva a mais de um destino
    for destinos in tabela.values():
        if len(destinos) > 1:
            return True
    
    # Verifica se algum estado não tem transição para algum símbolo
    for estado in automato.estados:
        for simbolo in automato.alfabeto:
            if simbolo == '&':  # Ignora lambda
                continue
            if (estado, simbolo) not in tabela:
                # AFD completo precisa ter transição para todos os símbolos
                # Se não tem, tecnicamente é AFN (incompleto)
                return True
    
    return False


def menu_principal():
    """
    Menu principal do programa
    """
    automato_atual = None
    tipo_atual = None
    
    while True:
        print("\n" + "="*50)
        print("    CONVERSÃO E TESTE DE AUTÔMATOS")
        print("="*50)
        
        if automato_atual:
            print(f"Autômato carregado: {tipo_atual}")
            print(f"Estados: {len(automato_atual.estados)} | Alfabeto: {sorted(automato_atual.alfabeto)}")
        else:
            print("Nenhum autômato carregado")
        
        print("\n--- MENU ---")
        print("1. Carregar autômato de arquivo JSON")
        print("2. Converter autômato")
        print("3. Testar palavra (terminal)")
        print("4. Testar palavras (arquivo)")
        print("5. Salvar autômato atual")
        print("6. Exibir detalhes do autômato")
        print("0. Sair")
        print("-"*50)
        
        opcao = input("Escolha uma opção: ").strip()
        
        if opcao == "1":
            carregar_automato_menu()
            # Atualiza automato_atual
            caminho = input("Digite o caminho do arquivo: ").strip()
            try:
                automato_atual = Automato.carregar_json(caminho)
                tipo_atual = detectar_tipo_automato(automato_atual)
                print(f"✓ Autômato carregado com sucesso! Tipo: {tipo_atual}")
            except Exception as e:
                print(f"✗ Erro ao carregar: {e}")
        
        elif opcao == "2":
            if not automato_atual:
                print("✗ Carregue um autômato primeiro!")
            else:
                automato_atual, tipo_atual = menu_conversao(automato_atual, tipo_atual)
        
        elif opcao == "3":
            if not automato_atual:
                print("✗ Carregue um autômato primeiro!")
            else:
                testar_palavra_terminal(automato_atual)
        
        elif opcao == "4":
            if not automato_atual:
                print("✗ Carregue um autômato primeiro!")
            else:
                caminho = input("Digite o caminho do arquivo de palavras: ").strip()
                testar_palavras_arquivo(automato_atual, caminho)
        
        elif opcao == "5":
            if not automato_atual:
                print("✗ Carregue um autômato primeiro!")
            else:
                caminho = input("Digite o caminho para salvar (ex: saida.json): ").strip()
                try:
                    automato_atual.salvar_json(caminho)
                    print(f"✓ Autômato salvo em '{caminho}'!")
                except Exception as e:
                    print(f"✗ Erro ao salvar: {e}")
        
        elif opcao == "6":
            if not automato_atual:
                print("✗ Carregue um autômato primeiro!")
            else:
                exibir_detalhes(automato_atual, tipo_atual)
        
        elif opcao == "0":
            print("\nEncerrando programa... Até logo!")
            break
        
        else:
            print("✗ Opção inválida! Tente novamente.")


def carregar_automato_menu():
    """Submenu para carregar autômato"""
    print("\n--- Carregar Autômato ---")
    print("Exemplos disponíveis:")
    print("  - automatos/teste.json (AFD)")
    print("  - automatos/testelambda.json (AFN-Lambda)")
    print("  - automatos/testeafn.json (AFN)")


def detectar_tipo_automato(automato: Automato) -> str:
    """Detecta e retorna o tipo do autômato como string"""
    if '&' in automato.alfabeto:
        return "AFN-Lambda"
    elif verificar_nao_determinismo(automato):
        return "AFN"
    else:
        return "AFD"


def menu_conversao(automato: Automato, tipo_atual: str):
    """Menu de conversão de autômatos"""
    print("\n--- Converter Autômato ---")
    print(f"Tipo atual: {tipo_atual}")
    print("\nOpções de conversão:")
    print("1. AFN → AFD")
    print("2. AFD → AFN")
    print("3. AFN → AFN-Lambda")
    print("4. AFD → AFN-Lambda")
    print("0. Voltar")
    
    opcao = input("Escolha: ").strip()
    
    if opcao == "1":
        print("Convertendo AFN → AFD...")
        novo = afn_para_afd(automato)
        print("✓ Conversão concluída!")
        return novo, "AFD"
    
    elif opcao == "2":
        print("Convertendo AFD → AFN...")
        novo = afd_para_afn(automato)
        print("✓ Conversão concluída!")
        return novo, "AFN"
    
    elif opcao == "3":
        print("Convertendo AFN → AFN-Lambda...")
        novo = afn_para_afn_lambda(automato)
        print("✓ Conversão concluída!")
        return novo, "AFN-Lambda"
    
    elif opcao == "4":
        print("Convertendo AFD → AFN-Lambda...")
        novo = afd_para_afn_lambda(automato)
        print("✓ Conversão concluída!")
        return novo, "AFN-Lambda"
    
    elif opcao == "0":
        return automato, tipo_atual
    
    else:
        print("✗ Opção inválida!")
        return automato, tipo_atual


def exibir_detalhes(automato: Automato, tipo: str):
    """Exibe detalhes completos do autômato"""
    print("\n" + "="*50)
    print(f"  DETALHES DO AUTÔMATO ({tipo})")
    print("="*50)
    print(f"Alfabeto: {sorted(automato.alfabeto)}")
    print(f"Estados: {sorted(automato.estados)}")
    print(f"Estado(s) inicial(is): {sorted(automato.estados_iniciais)}")
    print(f"Estado(s) final(is): {sorted(automato.estados_finais)}")
    print(f"\nTransições ({len(automato.transicoes)}):")
    for origem, destino, simbolo in sorted(automato.transicoes):
        print(f"  {origem} --{simbolo}--> {destino}")
    print("="*50)


# Ponto de entrada do programa
if __name__ == "__main__":
    menu_principal()
           
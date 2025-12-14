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
    
    tabela_afn = afn.construir_tabela_transicoes() 
    estado_inicial_afd = frozenset(afn.fecho_lambda(afn.estados_iniciais, tabela_afn))
    
    estados_afd = set()
    transicoes_afd = []
    estados_finais_afd = set()
    
    fila = [estado_inicial_afd]
    processados = set()
    
    while fila:
        estado_atual = fila.pop(0)
        
        if estado_atual in processados:
            continue
        
        processados.add(estado_atual)
        estados_afd.add(estado_atual)
        
        if estado_atual & afn.estados_finais:
            estados_finais_afd.add(estado_atual)
            
        for simbolo in afn.alfabeto:
            if simbolo == '&':
                continue
            
        proximo_estado = afn.fecho_lambda(proximo_estado, tabela_afn)
        
        if proximo_estado:
            proximo_estado = frozenset(proximo_estado)
            
            transicoes_afd.append([
                converter_estado_afd_para_nome(estado_atual),
                converter_estado_afd_para_nome(proximo_estado),
                simbolo
            ])                       
            
            if proximo_estado not in processados:
                fila.append(proximo_estado)
                
    estados_afd_nomes = {converter_estado_afd_para_nome(e) for e in estados_afd}
    estados_finais_afd_nomes = {converter_estado_afd_para_nome(e) for e in estados_finais_afd}
    estado_inicial_afd_nome = converter_estado_afd_para_nome(estado_inicial_afd)

    alfabeto_afd = afn.alfabeto - {'&'}
    
    return Automato(
        alfabeto=list(alfabeto_afd),
        estados=list(estados_afd_nomes),
        estados_iniciais=[estado_inicial_afd_nome],
        estados_finais=list(estados_finais_afd_nomes),
        transicoes=transicoes_afd
    )


def converter_estado_afd_para_nome(estado_frozenset) -> str:

    if not estado_frozenset:
        return '{}'
    return '{' + ','.join(sorted(estado_frozenset)) + '}'           
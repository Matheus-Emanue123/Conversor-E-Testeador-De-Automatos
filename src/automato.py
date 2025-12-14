import json
from typing import List, Set, Dict, Tuple

class Automato:
    
    def __init__(self, alfabeto: List[str], estados: List[str], 
                 estados_iniciais: List[str], estados_finais: List[str],
                 transicoes: List[List[str]]):
        self.alfabeto = set(alfabeto)
        self.estados = set(estados)
        self.estados_iniciais = set(estados_iniciais)
        self.estados_finais = set(estados_finais)
        self.transicoes = transicoes
        
    @classmethod
    def carregar_json(cls, caminho: str):
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
        tabela = {}
        for origem, destino, simbolo in self.transicoes:
            chave = (origem, simbolo)
            if chave not in tabela:
                tabela[chave] = set()
            tabela[chave].add(destino)
        return tabela
    
    def fecho_lambda(self, estados: Set[str], tabela: Dict) -> Set[str]:
        fecho = set(estados)
        pilha = list(estados)
        
        while pilha:
            estado = pilha.pop()
            if (estado, '&') in tabela:
                for proximo in tabela[(estado, '&')]:
                    if proximo not in fecho:
                        fecho.add(proximo)
                        pilha.append(proximo)
        
        return fecho
    
    def aceita_palavra(self, palavra: str, eh_afd: bool = False) -> bool:
        tabela = self.construir_tabela_transicoes()
        estados_atuais = self.fecho_lambda(self.estados_iniciais, tabela)
        
        for simbolo in palavra:
            if simbolo not in self.alfabeto:
                return False
            
            proximos_estados = set()
            
            for estado in estados_atuais:
                if (estado, simbolo) in tabela:
                    proximos_estados.update(tabela[(estado, simbolo)])
            
            if not proximos_estados:
                return False
            
            estados_atuais = self.fecho_lambda(proximos_estados, tabela)
        
        return bool(estados_atuais & self.estados_finais)

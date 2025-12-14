from typing import List, Set, Dict
from automato import Automato

def minimizar_afd(afd: Automato) -> Automato:
    afd_alcancavel = remover_estados_inalcancaveis(afd)
    afd_minimizado = remover_estados_equivalentes(afd_alcancavel)
    return afd_minimizado


def remover_estados_inalcancaveis(afd: Automato) -> Automato:
    tabela = afd.construir_tabela_transicoes()
    
    alcancaveis = set()
    fila = list(afd.estados_iniciais)
    
    while fila:
        estado = fila.pop(0)
        
        if estado in alcancaveis:
            continue
        
        alcancaveis.add(estado)
        
        for simbolo in afd.alfabeto:
            if (estado, simbolo) in tabela:
                for destino in tabela[(estado, simbolo)]:
                    if destino not in alcancaveis:
                        fila.append(destino)
    
    novas_transicoes = [
        [origem, destino, simbolo]
        for origem, destino, simbolo in afd.transicoes
        if origem in alcancaveis and destino in alcancaveis
    ]
    
    novos_finais = [e for e in afd.estados_finais if e in alcancaveis]
    
    return Automato(
        alfabeto=list(afd.alfabeto),
        estados=list(alcancaveis),
        estados_iniciais=list(afd.estados_iniciais),
        estados_finais=novos_finais,
        transicoes=novas_transicoes
    )


def remover_estados_equivalentes(afd: Automato) -> Automato:
    tabela = afd.construir_tabela_transicoes()
    estados = list(afd.estados)
    
    finais = set(afd.estados_finais)
    nao_finais = set(estados) - finais
    
    particoes = []
    if finais:
        particoes.append(finais)
    if nao_finais:
        particoes.append(nao_finais)
    
    mudou = True
    while mudou:
        mudou = False
        novas_particoes = []
        
        for particao in particoes:
            sub_particoes = dividir_particao(particao, particoes, tabela, afd.alfabeto)
            
            if len(sub_particoes) > 1:
                mudou = True
            
            novas_particoes.extend(sub_particoes)
        
        particoes = novas_particoes
    
    return construir_afd_minimizado(afd, particoes, tabela)


def dividir_particao(particao: Set[str], particoes: List[Set[str]], 
                     tabela: Dict, alfabeto: Set[str]) -> List[Set[str]]:
    if len(particao) == 1:
        return [particao]
    
    grupos = {}
    
    for estado in particao:
        assinatura = []
        
        for simbolo in sorted(alfabeto):
            if simbolo == '&':
                continue
            
            destino = None
            if (estado, simbolo) in tabela:
                destinos = tabela[(estado, simbolo)]
                if destinos:
                    destino = next(iter(destinos))
            
            particao_destino = None
            for i, p in enumerate(particoes):
                if destino in p:
                    particao_destino = i
                    break
            
            assinatura.append(particao_destino)
        
        assinatura_tuple = tuple(assinatura)
        
        if assinatura_tuple not in grupos:
            grupos[assinatura_tuple] = set()
        
        grupos[assinatura_tuple].add(estado)
    
    return list(grupos.values())


def construir_afd_minimizado(afd: Automato, particoes: List[Set[str]], 
                             tabela: Dict) -> Automato:
    estado_para_representante = {}
    representantes = {}
    
    for particao in particoes:
        representante = min(particao)
        representantes[representante] = particao
        for estado in particao:
            estado_para_representante[estado] = representante
    
    novos_estados = list(representantes.keys())
    
    inicial_original = next(iter(afd.estados_iniciais))
    novo_inicial = estado_para_representante[inicial_original]
    
    novos_finais = set()
    for estado_final in afd.estados_finais:
        novos_finais.add(estado_para_representante[estado_final])
    
    transicoes_vistas = set()
    novas_transicoes = []
    
    for representante in novos_estados:
        estado_exemplo = representante
        
        for simbolo in afd.alfabeto:
            if simbolo == '&':
                continue
            
            if (estado_exemplo, simbolo) in tabela:
                destino_original = next(iter(tabela[(estado_exemplo, simbolo)]))
                destino_novo = estado_para_representante[destino_original]
                
                transicao = (representante, destino_novo, simbolo)
                if transicao not in transicoes_vistas:
                    transicoes_vistas.add(transicao)
                    novas_transicoes.append([representante, destino_novo, simbolo])
    
    return Automato(
        alfabeto=list(afd.alfabeto),
        estados=novos_estados,
        estados_iniciais=[novo_inicial],
        estados_finais=list(novos_finais),
        transicoes=novas_transicoes
    )

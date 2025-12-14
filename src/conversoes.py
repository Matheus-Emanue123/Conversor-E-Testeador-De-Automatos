from typing import List, Set, Dict
from automato import Automato

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
            
            proximo_estado = set()
            for estado_afn in estado_atual:
                if (estado_afn, simbolo) in tabela_afn:
                    proximo_estado.update(tabela_afn[(estado_afn, simbolo)])
            
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


def afd_para_afn(afd: Automato) -> Automato:
    return Automato(
        alfabeto=list(afd.alfabeto),
        estados=list(afd.estados),
        estados_iniciais=list(afd.estados_iniciais),
        estados_finais=list(afd.estados_finais),
        transicoes=afd.transicoes.copy()
    )


def afn_para_afn_lambda(afn: Automato) -> Automato:
    novos_estados = list(afn.estados)
    novas_transicoes = afn.transicoes.copy()
    novo_alfabeto = list(afn.alfabeto)
    
    if '&' not in novo_alfabeto:
        novo_alfabeto.append('&')
    
    tabela = afn.construir_tabela_transicoes()
    
    for estado_inicial in afn.estados_iniciais:
        for simbolo in afn.alfabeto:
            if (estado_inicial, simbolo) in tabela:
                for destino in tabela[(estado_inicial, simbolo)]:
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
    afn = afd_para_afn(afd)
    return afn_para_afn_lambda(afn)

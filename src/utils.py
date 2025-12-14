from automato import Automato

def verificar_nao_determinismo(automato: Automato) -> bool:
    tabela = automato.construir_tabela_transicoes()
    
    if len(automato.estados_iniciais) > 1:
        return True
    
    for destinos in tabela.values():
        if len(destinos) > 1:
            return True
    
    for estado in automato.estados:
        for simbolo in automato.alfabeto:
            if simbolo == '&':
                continue
            if (estado, simbolo) not in tabela:
                return True
    
    return False


def detectar_tipo_automato(automato: Automato) -> str:
    if '&' in automato.alfabeto:
        return "AFN-Lambda"
    elif verificar_nao_determinismo(automato):
        return "AFN"
    else:
        return "AFD"


def exibir_detalhes(automato: Automato, tipo: str):
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

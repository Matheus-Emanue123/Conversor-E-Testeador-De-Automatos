from automato import Automato
from utils import verificar_nao_determinismo

def testar_palavra_terminal(automato: Automato) -> None:
    print("\n--- Testar Palavra ---")
    palavra = input("Digite a palavra a ser testada (deixe vazio para palavra vazia): ")
    
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

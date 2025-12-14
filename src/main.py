from automato import Automato
from conversoes import afn_para_afd, afd_para_afn, afn_para_afn_lambda, afd_para_afn_lambda
from minimizacao import minimizar_afd
from testes import testar_palavra_terminal, testar_palavras_arquivo
from utils import detectar_tipo_automato, exibir_detalhes, verificar_nao_determinismo

def menu_principal():
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
        print("3. Minimizar autômato (AFD)")
        print("4. Testar palavra (terminal)")
        print("5. Testar palavras (arquivo)")
        print("6. Salvar autômato atual")
        print("7. Exibir detalhes do autômato")
        print("0. Sair")
        print("-"*50)
        
        opcao = input("Escolha uma opção: ").strip()
        
        if opcao == "1":
            carregar_automato_menu()
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
                if tipo_atual != "AFD":
                    print("⚠ Convertendo para AFD primeiro...")
                    automato_atual = afn_para_afd(automato_atual)
                    tipo_atual = "AFD"
                
                print("Minimizando AFD...")
                estados_antes = len(automato_atual.estados)
                automato_atual = minimizar_afd(automato_atual)
                estados_depois = len(automato_atual.estados)
                print(f"✓ Minimização concluída!")
                print(f"  Estados: {estados_antes} → {estados_depois}")
        
        elif opcao == "4":
            if not automato_atual:
                print("✗ Carregue um autômato primeiro!")
            else:
                testar_palavra_terminal(automato_atual)
        
        elif opcao == "5":
            if not automato_atual:
                print("✗ Carregue um autômato primeiro!")
            else:
                caminho = input("Digite o caminho do arquivo de palavras: ").strip()
                testar_palavras_arquivo(automato_atual, caminho)
        
        elif opcao == "6":
            if not automato_atual:
                print("✗ Carregue um autômato primeiro!")
            else:
                caminho = input("Digite o caminho para salvar (ex: saida.json): ").strip()
                try:
                    automato_atual.salvar_json(caminho)
                    print(f"✓ Autômato salvo em '{caminho}'!")
                except Exception as e:
                    print(f"✗ Erro ao salvar: {e}")
        
        elif opcao == "7":
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
    print("\n--- Carregar Autômato ---")
    print("Exemplos disponíveis:")
    print("  - automatos/teste.json (AFD)")
    print("  - automatos/testelambda.json (AFN-Lambda)")
    print("  - automatos/testeafn.json (AFN)")


def menu_conversao(automato: Automato, tipo_atual: str):
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


if __name__ == "__main__":
    menu_principal()
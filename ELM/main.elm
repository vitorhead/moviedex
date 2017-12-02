module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Http exposing (send, get)
import Json.Decode as Decode exposing (bool, at)
import Login as ModuloLogin
import Cadastro as ModuloCadastro
import BuscaFilme as ModuloBuscaFilme

type Pagina = Cadastro
            | Login
            | BuscaFilme
            | Root

type Click =  Busca
            | MeusFilmes

type Message =
      PgCadastro ModuloCadastro.Message
    | PgLogin ModuloLogin.Message
    | PgBuscaFilme ModuloBuscaFilme.Message
    | Mudar Pagina
    | SubmitAutenticacao Click
    | ResponseAutenticacao (Result Http.Error Bool)


type alias Model =
    {
     login      : ModuloLogin.Model
    ,cadastro   : ModuloCadastro.Model
    ,buscaFilme : ModuloBuscaFilme.Model
    ,janela     : Pagina
    ,acao       : Click
    }

init : (Model, Cmd Message)
init = ({login = ModuloLogin.Model "" "" "" (ModuloLogin.Retorno 0 (ModuloLogin.Mensagem "" 0 "")),
         cadastro = ModuloCadastro.Model "" "" "" "" "" (ModuloCadastro.Retorno 0 0) "",
         buscaFilme = ModuloBuscaFilme.init,
         janela = Root,
         acao = MeusFilmes}, Cmd.none)
      
        
getValidaAutenticacao : String -> Cmd Message
getValidaAutenticacao auth =
    let
        url = ("https://haskelleta-romefeller.c9users.io/cadastro/autenticacao/"++auth)
    in
        Http.send ResponseAutenticacao <| Http.get url (at ["resp"] Decode.bool)


update : Message -> Model -> (Model, Cmd Message)
update msg model =
    case msg of
        Mudar p -> ({model | janela = p}, Cmd.none)

        PgLogin p ->
          let 
            updt = ModuloLogin.update p model.login
          in
            ({model | login = Tuple.first updt}, Cmd.map PgLogin <| Tuple.second updt)

        PgCadastro p -> 
          let
            updt = ModuloCadastro.update p model.cadastro
          in
            ({model | cadastro = Tuple.first updt}, Cmd.map PgCadastro <| Tuple.second updt)
        
        PgBuscaFilme p ->
          let
            updt = ModuloBuscaFilme.update p model.buscaFilme
            --pegando model antigo
            oldModelBuscaFilme = Tuple.first updt
            --passando idCadastro do model do main pro model do buscaFilme
            newBF = {oldModelBuscaFilme | idCadLogado = model.login.ret.mensagem.idcadastro}
          in
            ({model | buscaFilme = newBF}, Cmd.map PgBuscaFilme <| Tuple.second updt)
        
        SubmitAutenticacao clicado ->
          ({model | acao = clicado}, getValidaAutenticacao <| String.filter (\x -> x /= '"') model.login.ret.mensagem.autenticacao)
          
        ResponseAutenticacao resp ->
          case resp of
            Err y -> ({model | janela = Root}, Cmd.none)
            Ok y -> 
              case y of
                True -> 
                      let
                        clicado = 
                          case model.acao of
                            Busca -> BuscaFilme
                            
                            MeusFilmes -> Root
                      in  
                        ({model | janela = clicado}, Cmd.none)    
                False ->
                        ({model | janela = Root}, Cmd.none)    
        

viewMainPage : Html Message
viewMainPage =  
       div [class "row"] 
        [
          div [class "sidebar col s12 m4 l3"]
          [
            div [class "lateral-principal"] 
            [
              ul []
              [
                li [] [text "NOME CHAMPS"]
                ,li [] 
                [
                  a [class "btn green", onClick <| SubmitAutenticacao Busca] [text "Buscar Filmes"]
                  -- a [class "btn green", onClick <| SubmitAutenticacao << (Mudar BuscaFilme)] [text "Buscar Filmes"]
                ]
                ,li [] 
                [
                  a [class "btn red"] [text "Deslogar"]
                ]
              ]
            ]
          ]
          ,div [class "col s12 m8 l9"] 
          [
            section []
            [
              h1 [] [text "NOME LISTA"]
              ,ul [class "lista"]
              [
                li []  -- CRIAR UM LI NESSE ESTILO PARA CADA FILME
                [
                  div [class "poster-filme"] 
                  [
                    img [src "" ] []
                  ]
                ]
                ,li []  -- CRIAR UM LI NESSE ESTILO PARA CADA FILME
                [
                  div [class "poster-filme"] 
                  [
                    img [src "" ] []
                  ]
                ]
              ] -- fim ul
            ] -- fim section
          ]
        ]


viewRoot : Html Message
viewRoot =  div []
  [
     section [class "apresentacao"]
      [
        h2 [] [text "MovieDex"]
        ,p [] [text "Lorem ipsum dolor sit amet, consectetur adipisicing elit. Labore, nihil."]
        ,div []
        [
          a [onClick (Mudar Cadastro) ,class "btn green"] [text "Cadastro"]
          ,a [class "btn", onClick (Mudar Login)] [text "Login"]
        ]
      ]
    ,section [id "quem-somos", class "container"]
      [
        div [class "row center-align"]
        [
          a [href "", class "col s12 m6"]
           [
            img [src "", class "responsive-img circle", alt "Ramon Github"] []
          ]
          ,p [class "col s12 m6"] [text "Lorem ipsum dolor sit amet, consectetur adipisicing elit. Laudantium sapiente recusandae quas quam natus veniam officia sit architecto nisi aspernatur?"]
        ]
        ,div [class "row center-align"]
      [
        a [href "https://github.com/vitorhead", class "col s12 m6"]
        [
          img [src "../static/images/vitor.jpeg", class "responsive-img circle", alt "Vitor Github"] []
        ]
        ,p [class "col s12 m6"] [text "Lorem ipsum dolor sit amet, consectetur adipisicing elit. Laudantium sapiente recusandae quas quam natus veniam officia sit architecto nisi aspernatur?"]
      ]
        ,div [class "row center-align"]
      [
        a [href "https://github.com/castylho", class "col s12 m6"]
        [
          img [src "../static/images/Yohann.jpg", class "responsive-img circle", alt "Github Ramon"] []
        ]
        ,p [class "col s12 m6"] [text "Lorem ipsum dolor sit amet,"]
      ]
      ]
  ]


view : Model -> Html Message
view model =
    let 
      deslogado : Html Message      
      deslogado =
        case model.janela of
            Login -> Html.map PgLogin <| ModuloLogin.view model.login

            Cadastro -> Html.map PgCadastro <| ModuloCadastro.view model.cadastro

            Root -> viewRoot
            
            BuscaFilme -> Html.map PgBuscaFilme <| ModuloBuscaFilme.view model.buscaFilme 
            
      logado : Html Message      
      logado =
          case model.janela of
            Login -> viewMainPage

            Cadastro -> viewMainPage

            Root -> viewMainPage
            
            BuscaFilme -> Html.map PgBuscaFilme <| ModuloBuscaFilme.view model.buscaFilme     
    in
     if model.login.ret.mensagem.autenticacao == "" then
      deslogado
     else
      div [] [
        text <| toString model.janela ++ " - " ++toString model.acao,
        logado
      ] 
 
 
main = program
    { init = init
    , view = view
    , update = update
    , subscriptions =  \_ -> Sub.none
    }

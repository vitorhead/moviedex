module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Login as ModuloLogin
import Cadastro as ModuloCadastro
import BuscaFilme as ModuloBuscaFilme
--import ConsultaFilme as ModuloConsultaFilme


type Pagina = Cadastro
            | Login
            | Root

type Message =
      PgCadastro ModuloCadastro.Message
    | PgLogin ModuloLogin.Message
    | PgBuscaFilme ModuloBuscaFilme.Message
    | Mudar Pagina
    -- | IdCadLogado Int


type alias Model =
    {
     login      : ModuloLogin.Model
    ,cadastro   : ModuloCadastro.Model
    ,buscaFilme : ModuloBuscaFilme.Model
    ,janela     : Pagina
    }

init : (Model, Cmd Message)
-- init = let 
--         login = ModuloLogin.Model "" "" "" (ModuloLogin.Cadastro "" "" "" "" "")  
--         cadastro = ModuloCadastro.Model "" "" "" "" "" (ModuloCadastro.Retorno 0 0) "" 
--         janela = Root
--       in
        --Model login cadastro janela, 
init = ({login = ModuloLogin.Model "" "" "" (ModuloLogin.Retorno 0 (ModuloLogin.Mensagem "" 0 "")),
         cadastro = ModuloCadastro.Model "" "" "" "" "" (ModuloCadastro.Retorno 0 0) "",
         buscaFilme = ModuloBuscaFilme.init,
         janela = Root}, Cmd.none)
      
        

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
            
        -- IdCadLogado s ->
        --   let
        --     oldIdCadastro = model.buscaFilme
        --     newIdCadastro = { oldIdCadastro | idCadLogado = s }
        --   in
        --   ({model | buscaFilme = newIdCadastro}, Cmd.none)
      


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
      escolhido =
        case model.janela of
            Login -> Html.map PgLogin <| ModuloLogin.view model.login

            Cadastro -> Html.map PgCadastro <| ModuloCadastro.view model.cadastro

            Root -> viewRoot
            
    in
      if model.login.ret.mensagem.autenticacao == "" then
        escolhido
      else
        let
          logado = div [] 
            [
               --button [] [onClick <| IdCadLogado model.login.ret.mensagem.idcadastro],
               Html.map PgBuscaFilme <| ModuloBuscaFilme.view model.buscaFilme 
            ]
        in
        logado
        
main = program
    { init = init
    , view = view
    , update = update
    , subscriptions =  \_ -> Sub.none
    }

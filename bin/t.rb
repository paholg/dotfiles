#!/usr/bin/env ruby

case $1 in
  staging1a)
    kubectl config use-context staging.us-east-2
    ;;
  staging2)
    kubectl config use-context staging.us-west-2
    ;;
  app1*)
    kubectl config use-context production.us-west-2
    ;;
  app2*)
    kubectl config use-context production.us-east-1
    ;;
  *)
    echo "Unknown bento: $1"
    exit 1
esac

if [ "$1" = 'ssh' ]; then
    kubectl -n $2 scale --replicas=1 deployment server-console
    kubectl -n $2 exec -it $(kubectl -n $2 get pods -lrails_console=true -o jsonpath='{.items[0].metadata.name}') bash
elif [ "$1" = 'console' ]; then
    kubectl -n $2 scale --replicas=1 deployment server-console
    kubectl -n $2 exec -it $(kubectl -n $2 get pods -lrails_console=true -o jsonpath='{.items[0].metadata.name}') bundle exec rails c
elif [ "$1" = 't' ]; then
    if [ "$2" = 'c' ]; then
        # kubectl -n traffic-cop exec -it $(kubectl -n traffic-cp get pods -lbento=)
        echo "poop"
    elif [ "$2" = 'c' ]; then
        echo "poop"
    else
        kubectl -n traffic-cop "${@:2}"
    fi
else
    kubectl "$@"
fi

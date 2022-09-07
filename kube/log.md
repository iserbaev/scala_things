---
kubectl apply -f namespace.yaml
//kubectl create ns lesson14

kubectl get ns

kubectl delete -f namespace.yaml

---
kubectl apply -f pod.yaml

kubectl get pod -n lesson14

kubectl port-forward  -n lesson14 static-web 8080:8080

kubectl describe pod -n lesson14

kubectl -n lesson14 port-forward pod/static-web 8080:8080

kubectl logs -f -n kube-system etcd-minikube

kubectl get pods -n lesson14 static-web -o yaml
kubectl exec -it -n lesson14 pod/static-web -c web sh

kubectl delete pod -n lesson14 static-web

---

kubectl apply -f service.yaml

kubectl get svc -n lesson14


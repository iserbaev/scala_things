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

---
#### Lesson 15 ####

kubectl create ns lesson15
kubectl apply -f deployment.yaml
kubectl get pods -n lesson15
kubectl delete pod -n lesson15 goapp-deployment-5cd649ddbc-b8f82 // will restart

kubectl create ns rolling
kubectl apply -f rolling.yaml
kubectl get pods -n rolling

kubectl create ns rolling
kubectl apply -f rolling.yaml
kubectl delete pod myapp // will restart
kubectl rollout restart deployment/myapp


kubectl apply -f job.yaml
kubectl get pods
kubectl logs -f pi-4xjfx

kubectl get deployments -n lesson15
kubectl describe deployment -n lesson15 goapp-deployment
kubectl scale deployment -n lesson15 goapp-deployment --replicas=2
kubectl get pods -n lesson15
kubectl delete -f deployment.yaml
kubectl delete deployment -n lesson15 goapp-deployment

---
#### Lesson 16 ####

minikube start
kubectl create ns lesson16
kubectl apply -f deployment_secrets.yaml
kubectl get pods -n lesson16

kubectl apply -f configMap-to-env.yaml
kubectl apply -f deployment_with-config-map.yaml

Порою КонфигМапы удобнее создавать из файла:
kubectl create cm test-config -n lesson16 --from-file=root-ca.pem  # Вы можете писать cm вместо configmap

kubectl apply -f pvc.yaml
kubectl apply -f deployment_with-pvc.yaml

kubectl delete deployment -n lesson16 goapp-deployment

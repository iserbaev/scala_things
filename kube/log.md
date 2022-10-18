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
#### Lesson 1.5 ####

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
#### Lesson 1.6 ####

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

### Lesson 2.1

minikube start
helm create test-chart
kubectl create ns tst-namespace
helm install my-helm-release  test-chart -n tst-namespace -f test-chart/values.yaml
kubectl get pods -n tst-namespace
helm uninstall my-helm-release -n tst-namespace

kubectl create ns dev-namespace
helm install my-dev-release  test-chart -n dev-namespace -f test-chart/dev.yaml
kubectl get pods -n dev-namespace

helm package test-chart
helm status my-helm-release
helm uninstall my-helm-release -n dev-namespace

# Добавляем репозиторий Хельм-Чартов
helm repo add grafana https://grafana.github.io/helm-charts
# Устанавливаем релиз grafana
helm install grafana  grafana/grafana
# Пьем чай, пока Под Графаны не будет Running
kubectl get pods -w
# Узнаем пароль от учетки admin
kubectl get secret --namespace default grafana -o jsonpath="{.data.admin-password}" | base64 --decode ; echo

# Делаем проброс портов и заходим в браузер на  localhost:3000
export POD_NAME=$(kubectl get pods --namespace default -l "app.kubernetes.io/name=grafana,app.kubernetes.io/instance=grafana" -o jsonpath="{.items[0].metadata.name}")
kubectl --namespace default port-forward $POD_NAME 3000

helm uninstall grafana -n default

### Lesson 2.2
minikube start
kubectl create ns lesson22
kubectl apply -f deployment.yaml
kubectl get pods -n lesson22
kubectl delete deployment -n lesson22 my-pod-http